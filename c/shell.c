/*
 * The imperative shell: an SDL3 window with a Vulkan swapchain that the
 * functional core presents 426x200 RGBA frames into.
 *
 * Exactly fourteen functions cross into Lean (see Dill/Shell.lean for the
 * full table): window/present/input/clock lifecycle, the sound mixer and
 * MIDI music, aspect and overlay control, and PNG decoding.
 *
 * The GPU does no rendering. Each frame: the staging buffer receives the
 * core's pixels, they are copied into a 426x200 image, and that image is
 * blitted (nearest-neighbor, aspect-scaled — widescreen cover by default,
 * 4:3 letterboxed under --classic) onto the swapchain image.
 */

#include <lean/lean.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_vulkan.h>
#include <vulkan/vulkan.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Platform split. macOS uses Apple frameworks for PNG decode (ImageIO /
 * CoreGraphics), MIDI music (AudioToolbox), and multicore compositing
 * (libdispatch). Linux uses libpng for PNG, a serial compositor, and has
 * no built-in GM synth, so music is a no-op there. */
#ifdef __APPLE__
#include <AudioToolbox/AudioToolbox.h>
#include <ImageIO/ImageIO.h>
#include <CoreGraphics/CoreGraphics.h>
#include <dispatch/dispatch.h>
#include <pthread.h> /* pthread_main_np, for the Cocoa main-thread check */
#else
#include <png.h>
#endif

#define FRAME_W 426
#define FRAME_H 200
/* the classic 4:3 view is the middle 320 columns of the wide frame */
#define CLASSIC_W 320
#define FRAME_BYTES (FRAME_W * FRAME_H * 4)
/* Generous: drivers typically create 2-4 images. Acquire can hand back any
 * index up to the count the driver chose, so a truncated image array is
 * unsound — a swapchain with more images than this fails outright (see
 * create_swapchain) rather than presenting into slots we never filled. */
#define MAX_SWAPCHAIN_IMAGES 16

static SDL_Window *window;
static VkInstance instance;
static VkSurfaceKHR surface;
static VkPhysicalDevice physical;
static uint32_t queueFamily;
static VkDevice device;
static VkQueue queue;

static VkSwapchainKHR swapchain;
static VkImage swapchainImages[MAX_SWAPCHAIN_IMAGES];
static uint32_t swapchainImageCount;
static VkExtent2D swapchainExtent;

static VkCommandPool cmdPool;
static VkCommandBuffer cmd;
static VkFence inFlight;
static VkSemaphore acquireSem;
/* One render-finished semaphore per swapchain image, indexed by the acquired
 * image index (created in create_swapchain). A single reusable binary
 * semaphore would be re-signaled while vkQueuePresentKHR's wait on it from
 * the previous frame may still be pending — nothing observable marks that
 * wait complete, so re-use is invalid per the Vulkan spec. An image handed
 * back by acquire *has* finished its previous present, so its own semaphore
 * is free again by construction. */
static VkSemaphore renderSems[MAX_SWAPCHAIN_IMAGES];

static VkBuffer staging;
static VkDeviceMemory stagingMem;
static void *stagingPtr;
static VkImage frameImage; /* 426x200 RGBA, the blit source */
static VkDeviceMemory frameImageMem;

/* Input state, reported to Lean as a packed snapshot; see Dill/Shell.lean. */
/* Some bindings are aliases sharing one snapshot bit — W and Up both drive
 * KEY_FORWARD, either Shift is KEY_RUN, Ctrl and the left mouse button both
 * fire, Return and keypad Enter are one KEY_ENTER — so a single bitmask
 * maintained straight from key-down/key-up events would let releasing one
 * alias cancel the other while it is still held. Instead each physical
 * source is tracked on its own (`scanHeld` per scancode, the mouse button
 * separately) and `heldKeys` is rebuilt from them every poll. */
static bool scanHeld[SDL_SCANCODE_COUNT];
static bool mouseFireHeld; /* left mouse button, an alias of Ctrl's KEY_FIRE */
static uint64_t heldKeys;  /* the ORed snapshot bits, rebuilt each poll */
/* Keys that went *down* since the last poll, whether or not they are still
 * held. The snapshot is otherwise level-triggered — it reports what is held
 * at the instant Lean asks — so a key pressed and released between two polls
 * would never appear in any snapshot at all, and the press would simply be
 * lost. A frame is 16 ms at 60 Hz and a brisk tap is quicker than that, so
 * this is reachable in ordinary play, not just in theory. ORing this into
 * one snapshot and clearing it makes such a tap show up for exactly one
 * poll, which is what the game's edge detection wants anyway. */
static uint64_t tappedKeys;
/* Weapon keys are latched here rather than reported straight out of the key
 * bitmask. The snapshot carries a single weapon slot (`Input.weapon` is one
 * `Option Nat`, and `Input.decode` resolves the bits with an if/else chain),
 * so two weapon keys pressed inside one poll would collapse to the
 * lower-numbered one — and `tappedKeys` is cleared whether or not the
 * snapshot could carry what was in it, so the other press was simply lost.
 * Rolling a finger across 3 and 4 inside one 16 ms frame is enough to do it.
 * Queueing hands each press out on its own poll, in the order typed.
 *
 * A key merely *held* still reports every poll, from `heldKeys` as before:
 * that is what lets a weapon pressed during an attack take effect when the
 * gun finally comes free, since `tickWeapon` ignores the request until then. */
#define WEAPON_QUEUE_LEN 8
static uint8_t weaponQueue[WEAPON_QUEUE_LEN];
static int weaponQueueHead, weaponQueueCount;
static float mouseDx;
static bool quitRequested;
static bool classicAspect; /* crop to 4:3 instead of full 16:9 */
static VkFormat swapFormat; /* the chosen swapchain pixel format */
static bool presentContain; /* letterbox the whole frame (menus) vs cover */
/* Set when a present failed partway through a frame, after the swapchain
 * image was acquired. `acquireSem` is then pending a signal nothing will
 * ever wait on, and there is no cheap way to withdraw that — so rather than
 * acquire a second image on top of it, every later present is dropped.
 *
 * `main` catches the IO error and tears down, so in practice this is never
 * called again. But "never called again" was an unwritten invariant on the
 * caller, and one an added `try` in the game loop would quietly break; this
 * makes it a property of the shell instead. */
static bool presentBroken;

/* Optional full-resolution overlay: decoded to the swapchain size and
 * composited (in software, at native res) over the upscaled game frame
 * each present, so it never passes through the 426x200 framebuffer. */
static bool overlayActive;
static char overlayPath[1024];
static uint8_t *overlayRGBA;         /* overlayW*overlayH*4, premult RGBA */
static int overlayW, overlayH;       /* the overlay's native pixel size */
static VkBuffer presentBuf;          /* host buffer copied straight to swapchain */
static VkDeviceMemory presentMem;
static void *presentPtr;
static VkDeviceSize presentCap;      /* bytes currently allocated */
static void refresh_overlay(void);
static void composite_overlay(const uint8_t *game);
static void present_rect(int32_t winW, int32_t winH, int32_t *srcX0,
                         int32_t *srcX1, int32_t *srcY0, int32_t *srcY1,
                         int32_t *ox, int32_t *oy, int32_t *dstW, int32_t *dstH);
/* letters/digits typed since the last poll, for cheat codes */
static uint64_t typedQueue;
static int typedCount;

/* Sound: raw 8-bit mono clips loaded at startup, mixed by binding one
 * SDL audio stream per channel to the default output. */
#define MAX_SOUNDS 128
#define NUM_CHANNELS 8
/* The chainsaw's four voices (DSSAWUP/IDL/FUL/HIT) share one reserved channel
 * so a new one halts the last instead of piling up — vanilla plays all weapon
 * sounds on the player's single channel. Ids match the sound-table order in
 * Dill/Game/Sfx.lean (sawUp..sawHit = 62..65). World sounds skip this channel
 * and still overlap freely on the other seven. */
#define SAW_SFX_FIRST 62
#define SAW_SFX_LAST  65
#define SAW_CHANNEL   0
static SDL_AudioStream *audioChannels[NUM_CHANNELS];
static struct {
    uint8_t *data;
    int len;
    int rate;
} soundClips[MAX_SOUNDS];
static bool audioReady;

/* Music: a converted MIDI file played through the system's GM synth.
 * macOS only — Linux has no built-in synth, so music is silently skipped. */
#ifdef __APPLE__
static MusicPlayer musicPlayer;
static MusicSequence musicSeq;
#endif

/* macOS windowing (Cocoa) only works from the process main thread, but Lean
 * normally runs `main` on a worker thread for a large stack. This constructor
 * runs before Lean's entry point reads the variable, forcing main onto the
 * process main thread. That trades away the big worker stack for the default
 * main stack, which has proven sufficient (the toolchain's lld does not
 * implement -Wl,-stack_size, so enlarging it back is not an option — see the
 * lakefile). On Linux SDL works from Lean's worker thread, so we leave the
 * big-stacked worker in place and skip this. */
#ifdef __APPLE__
__attribute__((constructor)) static void dill_use_main_thread(void) {
    setenv("LEAN_MAIN_USE_THREAD", "0", 1);
}
#endif

#ifdef __APPLE__
/* True if the calling thread is the process's initial (main) thread, which
 * Cocoa windowing requires. Only meaningful on macOS (see dill_init). */
static bool on_main_thread(void) {
    return pthread_main_np() != 0;
}
#endif

static lean_obj_res io_err(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

static lean_obj_res io_ok(void) {
    return lean_io_result_mk_ok(lean_box(0));
}

#define CHECK_VK(expr, what)                                                  \
    do {                                                                      \
        VkResult vr_ = (expr);                                                \
        if (vr_ != VK_SUCCESS) {                                              \
            static char buf_[256];                                            \
            snprintf(buf_, sizeof buf_, "%s failed (VkResult %d)", what,      \
                     (int)vr_);                                               \
            return io_err(buf_);                                              \
        }                                                                     \
    } while (0)

/* `CHECK_VK` for the inside of `dill_present`: a failure there abandons a
 * frame that already owns GPU state (see `presentBroken`), so it retires
 * presenting altogether rather than leaving the next call to walk into it. */
#define CHECK_VK_FRAME(expr, what)                                            \
    do {                                                                      \
        VkResult vr_ = (expr);                                                \
        if (vr_ != VK_SUCCESS) {                                              \
            static char buf_[256];                                            \
            snprintf(buf_, sizeof buf_, "%s failed (VkResult %d)", what,      \
                     (int)vr_);                                               \
            presentBroken = true;                                             \
            return io_err(buf_);                                              \
        }                                                                     \
    } while (0)

static uint32_t find_memory_type(uint32_t typeBits, VkMemoryPropertyFlags props) {
    VkPhysicalDeviceMemoryProperties mem;
    vkGetPhysicalDeviceMemoryProperties(physical, &mem);
    for (uint32_t i = 0; i < mem.memoryTypeCount; i++)
        if ((typeBits & (1u << i)) &&
            (mem.memoryTypes[i].propertyFlags & props) == props)
            return i;
    return UINT32_MAX;
}

static VkResult create_swapchain(void) {
    VkSurfaceCapabilitiesKHR caps;
    VkResult r = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &caps);
    if (r != VK_SUCCESS) return r;

    /* Choose the swapchain format deterministically so the picture is identical
     * on every driver (MoltenVK and Linux alike). The Doom palette is already
     * sRGB-encoded and must be shown *directly*: a plain 8-bit UNORM surface in
     * the standard sRGB colour space does exactly that. We must avoid an _SRGB
     * swapchain, whose blit/scanout re-applies a linear->sRGB gamma curve and
     * washes the palette out — that mismatch (one platform landing on UNORM,
     * the other on SRGB) is what made macOS and Linux look different. Pinning
     * the colour space to SRGB_NONLINEAR also stops macOS from treating the
     * frame as wide-gamut (P3) and oversaturating it.
     *
     * Preference order: BGRA8/RGBA8 UNORM in sRGB space; then any 8-bit UNORM;
     * then whatever is first (last resort — an all-sRGB surface is vanishingly
     * rare and still renders, just with the old gamma). */
    VkSurfaceFormatKHR formats[64];
    uint32_t formatCount = 64;
    r = vkGetPhysicalDeviceSurfaceFormatsKHR(physical, surface, &formatCount,
                                             formats);
    /* VK_INCOMPLETE just means the surface offered more than 64 formats;
       the 64 we did receive are plenty to choose from. */
    if (r != VK_SUCCESS && r != VK_INCOMPLETE) return r;
    if (formatCount == 0) return VK_ERROR_FORMAT_NOT_SUPPORTED;
    VkSurfaceFormatKHR chosen = formats[0];
    bool picked = false;
    for (uint32_t i = 0; i < formatCount && !picked; i++) {
        bool unorm = formats[i].format == VK_FORMAT_B8G8R8A8_UNORM ||
                     formats[i].format == VK_FORMAT_R8G8B8A8_UNORM;
        if (unorm &&
            formats[i].colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
            chosen = formats[i];
            picked = true;
        }
    }
    for (uint32_t i = 0; i < formatCount && !picked; i++)
        if (formats[i].format == VK_FORMAT_B8G8R8A8_UNORM ||
            formats[i].format == VK_FORMAT_R8G8B8A8_UNORM) {
            chosen = formats[i];
            picked = true;
        }
    swapFormat = chosen.format;

    /* Some window systems (notably Wayland) leave the swapchain extent up to
     * the application by returning UINT32_MAX in currentExtent. Passing that
     * sentinel to vkCreateSwapchainKHR asks the driver for an impossibly large
     * image and can surface as VK_ERROR_OUT_OF_HOST_MEMORY. Use SDL's drawable
     * pixel size in that case, then respect the surface's advertised limits. */
    if (caps.currentExtent.width != UINT32_MAX) {
        swapchainExtent = caps.currentExtent;
    } else {
        int pixelWidth, pixelHeight;
        if (!SDL_GetWindowSizeInPixels(window, &pixelWidth, &pixelHeight) ||
            pixelWidth <= 0 || pixelHeight <= 0)
            return VK_ERROR_INITIALIZATION_FAILED;

        swapchainExtent.width = (uint32_t)pixelWidth;
        swapchainExtent.height = (uint32_t)pixelHeight;
        if (swapchainExtent.width < caps.minImageExtent.width)
            swapchainExtent.width = caps.minImageExtent.width;
        if (swapchainExtent.width > caps.maxImageExtent.width)
            swapchainExtent.width = caps.maxImageExtent.width;
        if (swapchainExtent.height < caps.minImageExtent.height)
            swapchainExtent.height = caps.minImageExtent.height;
        if (swapchainExtent.height > caps.maxImageExtent.height)
            swapchainExtent.height = caps.maxImageExtent.height;
    }
    uint32_t minImages = caps.minImageCount + 1;
    if (caps.maxImageCount > 0 && minImages > caps.maxImageCount)
        minImages = caps.maxImageCount;

    VkSwapchainKHR old = swapchain;
    VkSwapchainCreateInfoKHR sci = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = surface,
        .minImageCount = minImages,
        .imageFormat = chosen.format,
        .imageColorSpace = chosen.colorSpace,
        .imageExtent = swapchainExtent,
        .imageArrayLayers = 1,
        .imageUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        .imageSharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .preTransform = caps.currentTransform,
        .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = VK_PRESENT_MODE_FIFO_KHR, /* vsync */
        .clipped = VK_TRUE,
        .oldSwapchain = old,
    };
    r = vkCreateSwapchainKHR(device, &sci, NULL, &swapchain);
    if (old) vkDestroySwapchainKHR(device, old, NULL);
    if (r != VK_SUCCESS) {
        /* The spec leaves the output handle undefined on failure — it may
         * still hold `old`, which was just destroyed. Null it explicitly so
         * dill_present's swapchain guard actually sees the swapchain gone. */
        swapchain = VK_NULL_HANDLE;
        return r;
    }

    /* Query the count first: fetching straight into the fixed array would
     * return VK_INCOMPLETE for an oversized swapchain, and acquire could
     * then hand back an index past the slots we actually filled. */
    swapchainImageCount = 0;
    r = vkGetSwapchainImagesKHR(device, swapchain, &swapchainImageCount, NULL);
    if (r == VK_SUCCESS && swapchainImageCount > MAX_SWAPCHAIN_IMAGES)
        r = VK_ERROR_TOO_MANY_OBJECTS;
    if (r == VK_SUCCESS)
        r = vkGetSwapchainImagesKHR(device, swapchain, &swapchainImageCount,
                                    swapchainImages);
    if (r != VK_SUCCESS) {
        /* Leave no half-initialised swapchain behind: destroy it and null
         * the handle, as the failed-create path above does, so
         * `dill_present`'s null guard sees the swapchain gone instead of
         * presenting into stale or overflowed image data. */
        vkDestroySwapchainKHR(device, swapchain, NULL);
        swapchain = VK_NULL_HANDLE;
        swapchainImageCount = 0;
        return r;
    }
    /* The per-image render-finished semaphores live and die with the
     * swapchain (see their declaration). Destroying the old set is safe
     * here: dill_init has no frames in flight yet, and recreate_swapchain
     * waits the device idle first. */
    for (uint32_t i = 0; i < MAX_SWAPCHAIN_IMAGES; i++)
        if (renderSems[i]) {
            vkDestroySemaphore(device, renderSems[i], NULL);
            renderSems[i] = VK_NULL_HANDLE;
        }
    VkSemaphoreCreateInfo rsci = {.sType =
                                      VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
    for (uint32_t i = 0; i < swapchainImageCount; i++) {
        r = vkCreateSemaphore(device, &rsci, NULL, &renderSems[i]);
        if (r != VK_SUCCESS) {
            /* as the image-fetch failure above: leave nothing half-built */
            renderSems[i] = VK_NULL_HANDLE; /* handle undefined on failure */
            for (uint32_t j = 0; j < i; j++) {
                vkDestroySemaphore(device, renderSems[j], NULL);
                renderSems[j] = VK_NULL_HANDLE;
            }
            vkDestroySwapchainKHR(device, swapchain, NULL);
            swapchain = VK_NULL_HANDLE;
            swapchainImageCount = 0;
            return r;
        }
    }
    refresh_overlay(); /* re-decode + resize the overlay for the new extent */
    return r;
}

static void recreate_swapchain(void) {
    vkDeviceWaitIdle(device);
    /* On failure `create_swapchain` has already destroyed the old swapchain
     * and left `swapchain` null — it does not roll back. That happens for
     * real when the surface goes away under us, which is precisely what a
     * window being closed looks like. `dill_present` therefore has to check
     * for a null swapchain before touching it rather than assume this
     * succeeded; passing VK_NULL_HANDLE to vkAcquireNextImageKHR is
     * undefined behaviour and MoltenVK takes it badly. */
    (void)create_swapchain();
}

LEAN_EXPORT lean_obj_res dill_init(uint32_t width, uint32_t height,
                                   b_lean_obj_arg title, lean_obj_arg world) {
    (void)world;
#ifdef __APPLE__
    /* Cocoa requires the process main thread; the constructor above forced
     * `main` onto it. On Linux SDL is happy on Lean's worker thread. */
    if (!on_main_thread())
        return io_err("dill_init: not on the process main thread; "
                      "macOS windowing requires it");
#endif
    if (!SDL_Init(SDL_INIT_VIDEO))
        return io_err(SDL_GetError());

    /* Sound is a nice-to-have: init failure just means silence. */
    if (SDL_Init(SDL_INIT_AUDIO)) {
        SDL_AudioSpec spec = {SDL_AUDIO_U8, 1, 11025};
        audioReady = true;
        for (int i = 0; i < NUM_CHANNELS; i++) {
            audioChannels[i] = SDL_OpenAudioDeviceStream(
                SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, NULL, NULL);
            if (!audioChannels[i]) {
                audioReady = false;
                break;
            }
            SDL_ResumeAudioStreamDevice(audioChannels[i]);
        }
    }

    window = SDL_CreateWindow(lean_string_cstr(title), (int)width, (int)height,
                              SDL_WINDOW_VULKAN | SDL_WINDOW_HIGH_PIXEL_DENSITY |
                                  SDL_WINDOW_FULLSCREEN);
    if (!window)
        return io_err(SDL_GetError());
    SDL_SetWindowRelativeMouseMode(window, true);

    /* Instance: SDL's required extensions, plus MoltenVK's portability
     * enumeration *when the loader offers it*. MoltenVK (macOS) requires it;
     * native Linux/Windows drivers don't provide it, and requesting an
     * absent instance extension fails vkCreateInstance — so enable it (and
     * its create flag) only if it is actually available. */
    VkExtensionProperties availInstExts[512];
    uint32_t availInstExtCount = 512;
    VkResult instExtR = vkEnumerateInstanceExtensionProperties(
        NULL, &availInstExtCount, availInstExts);
    /* VK_INCOMPLETE only means the loader offers more than 512 extensions;
       the 512 we did receive are plenty to search. On any other failure the
       buffer holds nothing we may read and `availInstExtCount` is undefined,
       so search an empty list rather than 512 uninitialised name arrays. */
    if (instExtR != VK_SUCCESS && instExtR != VK_INCOMPLETE)
        availInstExtCount = 0;
    bool hasPortability = false, hasGetPhysProps2 = false;
    for (uint32_t i = 0; i < availInstExtCount; i++) {
        const char *n = availInstExts[i].extensionName;
        if (strcmp(n, VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME) == 0)
            hasPortability = true;
        if (strcmp(n, VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME) == 0)
            hasGetPhysProps2 = true;
    }

    uint32_t sdlExtCount = 0;
    const char *const *sdlExts = SDL_Vulkan_GetInstanceExtensions(&sdlExtCount);
    const char *exts[16];
    uint32_t extCount = 0;
    for (uint32_t i = 0; i < sdlExtCount && extCount < 14; i++)
        exts[extCount++] = sdlExts[i];
    VkInstanceCreateFlags instFlags = 0;
    if (hasPortability) {
        exts[extCount++] = VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME;
        instFlags |= VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR;
    }
    if (hasGetPhysProps2)
        exts[extCount++] = VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME;

    VkApplicationInfo app = {
        .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = "dill",
        .apiVersion = VK_API_VERSION_1_1,
    };
    VkInstanceCreateInfo ici = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .flags = instFlags,
        .pApplicationInfo = &app,
        .enabledExtensionCount = extCount,
        .ppEnabledExtensionNames = exts,
    };
    CHECK_VK(vkCreateInstance(&ici, NULL, &instance), "vkCreateInstance");

    if (!SDL_Vulkan_CreateSurface(window, instance, NULL, &surface))
        return io_err(SDL_GetError());

    /* Pick the first device whose queue family does graphics and present. */
    VkPhysicalDevice devices[16];
    uint32_t deviceCount = 16;
    VkResult er = vkEnumeratePhysicalDevices(instance, &deviceCount, devices);
    /* VK_INCOMPLETE means more than 16 devices exist; the 16 we got are
       plenty of candidates, so treat it as success. */
    if (er != VK_SUCCESS && er != VK_INCOMPLETE) {
        static char ebuf[64];
        snprintf(ebuf, sizeof ebuf,
                 "vkEnumeratePhysicalDevices failed (VkResult %d)", (int)er);
        return io_err(ebuf);
    }
    physical = VK_NULL_HANDLE;
    for (uint32_t d = 0; d < deviceCount && !physical; d++) {
        VkQueueFamilyProperties families[32];
        uint32_t familyCount = 32;
        vkGetPhysicalDeviceQueueFamilyProperties(devices[d], &familyCount,
                                                 families);
        for (uint32_t f = 0; f < familyCount; f++) {
            VkBool32 present = VK_FALSE;
            vkGetPhysicalDeviceSurfaceSupportKHR(devices[d], f, surface,
                                                 &present);
            if (present && (families[f].queueFlags & VK_QUEUE_GRAPHICS_BIT)) {
                physical = devices[d];
                queueFamily = f;
                break;
            }
        }
    }
    if (!physical)
        return io_err("no Vulkan device can draw to the window");

    /* Device. VK_KHR_portability_subset must be enabled when offered. */
    const char *devExts[2] = {VK_KHR_SWAPCHAIN_EXTENSION_NAME};
    uint32_t devExtCount = 1;
    VkExtensionProperties available[512];
    uint32_t availableCount = 512;
    VkResult devExtR = vkEnumerateDeviceExtensionProperties(
        physical, NULL, &availableCount, available);
    /* VK_INCOMPLETE only means the driver offers more than 512 extensions;
       the 512 we did receive are plenty to search. On any other failure the
       buffer holds nothing we may read and `availableCount` is undefined, so
       search an empty list rather than 512 uninitialised name arrays. */
    if (devExtR != VK_SUCCESS && devExtR != VK_INCOMPLETE)
        availableCount = 0;
    for (uint32_t i = 0; i < availableCount; i++)
        if (strcmp(available[i].extensionName, "VK_KHR_portability_subset") == 0) {
            devExts[devExtCount++] = "VK_KHR_portability_subset";
            /* `devExts` has room for exactly this one extra name, and a
               driver listing the extension twice would write past it. */
            break;
        }

    float priority = 1.0f;
    VkDeviceQueueCreateInfo qci = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        .queueFamilyIndex = queueFamily,
        .queueCount = 1,
        .pQueuePriorities = &priority,
    };
    VkDeviceCreateInfo dci = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .queueCreateInfoCount = 1,
        .pQueueCreateInfos = &qci,
        .enabledExtensionCount = devExtCount,
        .ppEnabledExtensionNames = devExts,
    };
    CHECK_VK(vkCreateDevice(physical, &dci, NULL, &device), "vkCreateDevice");
    vkGetDeviceQueue(device, queueFamily, 0, &queue);

    CHECK_VK(create_swapchain(), "swapchain creation");

    /* Staging buffer the core's frames are copied into, mapped forever. */
    VkBufferCreateInfo bci = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = FRAME_BYTES,
        .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };
    CHECK_VK(vkCreateBuffer(device, &bci, NULL, &staging), "vkCreateBuffer");
    VkMemoryRequirements bufReq;
    vkGetBufferMemoryRequirements(device, staging, &bufReq);
    /* find_memory_type's UINT32_MAX sentinel must be caught before it goes
     * into a VkMemoryAllocateInfo: an out-of-range memoryTypeIndex is
     * undefined behaviour in vkAllocateMemory, not a reportable error. */
    uint32_t stagingType =
        find_memory_type(bufReq.memoryTypeBits,
                         VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                             VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
    if (stagingType == UINT32_MAX)
        return io_err("no host-visible memory type for the staging buffer");
    VkMemoryAllocateInfo bai = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = bufReq.size,
        .memoryTypeIndex = stagingType,
    };
    CHECK_VK(vkAllocateMemory(device, &bai, NULL, &stagingMem),
             "staging vkAllocateMemory");
    CHECK_VK(vkBindBufferMemory(device, staging, stagingMem, 0),
             "vkBindBufferMemory");
    CHECK_VK(vkMapMemory(device, stagingMem, 0, FRAME_BYTES, 0, &stagingPtr),
             "vkMapMemory");

    /* The 426x200 image the buffer is copied into (blit needs an image
       source; the blit also converts RGBA -> swapchain BGRA). */
    VkImageCreateInfo imgci = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .imageType = VK_IMAGE_TYPE_2D,
        .format = VK_FORMAT_R8G8B8A8_UNORM,
        .extent = {FRAME_W, FRAME_H, 1},
        .mipLevels = 1,
        .arrayLayers = 1,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .usage = VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
                 VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
    };
    CHECK_VK(vkCreateImage(device, &imgci, NULL, &frameImage), "vkCreateImage");
    VkMemoryRequirements imgReq;
    vkGetImageMemoryRequirements(device, frameImage, &imgReq);
    /* sentinel check as for the staging buffer above */
    uint32_t imageType = find_memory_type(imgReq.memoryTypeBits,
                                          VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
    if (imageType == UINT32_MAX)
        return io_err("no device-local memory type for the frame image");
    VkMemoryAllocateInfo iai = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = imgReq.size,
        .memoryTypeIndex = imageType,
    };
    CHECK_VK(vkAllocateMemory(device, &iai, NULL, &frameImageMem),
             "image vkAllocateMemory");
    CHECK_VK(vkBindImageMemory(device, frameImage, frameImageMem, 0),
             "vkBindImageMemory");

    /* One command buffer, one frame in flight. */
    VkCommandPoolCreateInfo pci = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = queueFamily,
    };
    CHECK_VK(vkCreateCommandPool(device, &pci, NULL, &cmdPool),
             "vkCreateCommandPool");
    VkCommandBufferAllocateInfo cai = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = cmdPool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1,
    };
    CHECK_VK(vkAllocateCommandBuffers(device, &cai, &cmd),
             "vkAllocateCommandBuffers");

    VkFenceCreateInfo fci = {.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
                             .flags = VK_FENCE_CREATE_SIGNALED_BIT};
    CHECK_VK(vkCreateFence(device, &fci, NULL, &inFlight), "vkCreateFence");
    VkSemaphoreCreateInfo semci = {.sType =
                                       VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
    CHECK_VK(vkCreateSemaphore(device, &semci, NULL, &acquireSem),
             "vkCreateSemaphore");
    /* the render-finished semaphores are per swapchain image and were
     * created with the swapchain above */
    return io_ok();
}

/* Transition an image between layouts with an all-commands barrier.
   Coarse but plenty for two transfers per 60 Hz frame. */
static void barrier(VkImage image, VkImageLayout from, VkImageLayout to) {
    VkImageMemoryBarrier b = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask = VK_ACCESS_MEMORY_WRITE_BIT,
        .dstAccessMask = VK_ACCESS_MEMORY_READ_BIT | VK_ACCESS_MEMORY_WRITE_BIT,
        .oldLayout = from,
        .newLayout = to,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .image = image,
        .subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1},
    };
    vkCmdPipelineBarrier(cmd, VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                         VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, 0, 0, NULL, 0,
                         NULL, 1, &b);
}

LEAN_EXPORT lean_obj_res dill_present(b_lean_obj_arg frame,
                                      lean_obj_arg world) {
    (void)world;
    if (lean_sarray_size(frame) < FRAME_BYTES)
        return io_err("dill_present: frame smaller than 426x200 RGBA");
    /* No swapchain means a recreate failed — the surface is going away, as
     * when the window is closing. Drop the frame instead of presenting into
     * a null handle; the next recreate picks things up if it comes back. */
    if (!device || presentBroken)
        return io_ok();
    if (swapchain == VK_NULL_HANDLE) {
        recreate_swapchain();
        return io_ok();
    }

    CHECK_VK_FRAME(vkWaitForFences(device, 1, &inFlight, VK_TRUE, UINT64_MAX),
             "vkWaitForFences");

    uint32_t index;
    VkResult r = vkAcquireNextImageKHR(device, swapchain, UINT64_MAX,
                                       acquireSem, VK_NULL_HANDLE, &index);
    if (r == VK_ERROR_OUT_OF_DATE_KHR) {
        recreate_swapchain();
        return io_ok(); /* drop this frame */
    }
    if (r != VK_SUCCESS && r != VK_SUBOPTIMAL_KHR) {
        presentBroken = true;
        return io_err("vkAcquireNextImageKHR failed");
    }
    /* From here on the frame owns `acquireSem`, which is pending a signal
     * that nothing will wait on if anything below fails. There is no cheap
     * way to withdraw that, so `presentBroken` above makes sure we never
     * acquire a second one on top of it; `dill_shutdown`'s
     * `vkDeviceWaitIdle` is then what makes the semaphore safe to destroy.
     *
     * The fence needs no such care: it is reset immediately before the
     * submit that signals it (below), never here, so every failure path
     * leaves it signaled and a later `vkWaitForFences` — or the teardown —
     * cannot hang on a fence that was reset for a batch never submitted. */

    VkCommandBufferBeginInfo begin = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };
    CHECK_VK_FRAME(vkBeginCommandBuffer(cmd, &begin), "vkBeginCommandBuffer");

    /* The CPU compositor writes 4-byte RGBA/BGRA pixels, so the 1:1
     * buffer-to-image copy below only makes sense for an 8-bit-per-channel
     * 32-bit swapchain format. create_swapchain's last-resort fallback can
     * accept *any* format the surface offers, and a wider one (say 16 bits
     * per channel) would make the copy read past the end of the W*H*4
     * present buffer on the GPU. Fall back to the plain blit then — it
     * converts formats correctly — at the cost of no overlay. */
    bool overlayFormatOk = swapFormat == VK_FORMAT_R8G8B8A8_UNORM ||
                           swapFormat == VK_FORMAT_R8G8B8A8_SRGB ||
                           swapFormat == VK_FORMAT_B8G8R8A8_UNORM ||
                           swapFormat == VK_FORMAT_B8G8R8A8_SRGB;
    if (overlayActive && presentPtr && overlayFormatOk) {
        /* Native-resolution path: CPU-composite the upscaled game and the
         * full-res overlay into the present buffer, then copy it 1:1 onto
         * the swapchain. The overlay never touches the 426x200 image. */
        composite_overlay(lean_sarray_cptr(frame));
        barrier(swapchainImages[index], VK_IMAGE_LAYOUT_UNDEFINED,
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
        VkBufferImageCopy full = {
            .imageSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
            .imageExtent = {swapchainExtent.width, swapchainExtent.height, 1},
        };
        vkCmdCopyBufferToImage(cmd, presentBuf, swapchainImages[index],
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &full);
        barrier(swapchainImages[index], VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);
        CHECK_VK_FRAME(vkEndCommandBuffer(cmd), "vkEndCommandBuffer");
    } else {
    /* frame -> staging buffer (the overlay path above reads the frame
     * directly, so the copy would be dead work there) -> frame image */
    memcpy(stagingPtr, lean_sarray_cptr(frame), FRAME_BYTES);
    barrier(frameImage, VK_IMAGE_LAYOUT_UNDEFINED,
            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
    VkBufferImageCopy copy = {
        .imageSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
        .imageExtent = {FRAME_W, FRAME_H, 1},
    };
    vkCmdCopyBufferToImage(cmd, staging, frameImage,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &copy);
    barrier(frameImage, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);

    /* clear the swapchain image (letterbox bars), then blit 4:3 centered */
    barrier(swapchainImages[index], VK_IMAGE_LAYOUT_UNDEFINED,
            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
    VkClearColorValue black = {{0.0f, 0.0f, 0.0f, 1.0f}};
    VkImageSubresourceRange whole = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
    vkCmdClearColorImage(cmd, swapchainImages[index],
                         VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, &black, 1,
                         &whole);

    int32_t winW = (int32_t)swapchainExtent.width;
    int32_t winH = (int32_t)swapchainExtent.height;
    int32_t srcX0, srcX1, srcY0, srcY1, ox, oy, dstW, dstH;
    present_rect(winW, winH, &srcX0, &srcX1, &srcY0, &srcY1, &ox, &oy,
                 &dstW, &dstH);
    VkImageBlit blit = {
        .srcSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
        .srcOffsets = {{srcX0, srcY0, 0}, {srcX1, srcY1, 1}},
        .dstSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
        .dstOffsets = {{ox, oy, 0}, {ox + dstW, oy + dstH, 1}},
    };
    vkCmdBlitImage(cmd, frameImage, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                   swapchainImages[index],
                   VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &blit,
                   VK_FILTER_NEAREST);
    barrier(swapchainImages[index], VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);
    CHECK_VK_FRAME(vkEndCommandBuffer(cmd), "vkEndCommandBuffer");
    }

    VkPipelineStageFlags waitStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    VkSubmitInfo submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &acquireSem,
        .pWaitDstStageMask = &waitStage,
        .commandBufferCount = 1,
        .pCommandBuffers = &cmd,
        .signalSemaphoreCount = 1,
        .pSignalSemaphores = &renderSems[index],
    };
    /* reset as late as possible: see the note after the acquire */
    CHECK_VK_FRAME(vkResetFences(device, 1, &inFlight), "vkResetFences");
    CHECK_VK_FRAME(vkQueueSubmit(queue, 1, &submit, inFlight), "vkQueueSubmit");

    VkPresentInfoKHR present = {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &renderSems[index],
        .swapchainCount = 1,
        .pSwapchains = &swapchain,
        .pImageIndices = &index,
    };
    r = vkQueuePresentKHR(queue, &present);
    if (r == VK_ERROR_OUT_OF_DATE_KHR || r == VK_SUBOPTIMAL_KHR)
        recreate_swapchain();
    else if (r != VK_SUCCESS) {
        presentBroken = true;
        return io_err("vkQueuePresentKHR failed");
    }
    return io_ok();
}

/* Bit layout of the input snapshot; keep in sync with Input.decode. */
enum {
    KEY_FORWARD = 1 << 0,
    KEY_BACK = 1 << 1,
    KEY_STRAFE_LEFT = 1 << 2,
    KEY_STRAFE_RIGHT = 1 << 3,
    KEY_TURN_LEFT = 1 << 4,
    KEY_TURN_RIGHT = 1 << 5,
    KEY_RUN = 1 << 6,
    KEY_USE = 1 << 7,
    KEY_FIRE = 1 << 8,
    KEY_WEAPON1 = 1 << 9,
    KEY_WEAPON2 = 1 << 10,
    KEY_WEAPON3 = 1 << 11,
    KEY_WEAPON4 = 1 << 12,
    KEY_PAUSE = 1 << 13,
    FLAG_QUIT = 1 << 15,
    KEY_SAVE = 1 << 16,
    KEY_LOAD = 1 << 17,
    /* bit 18 is unassigned (a Q-to-quit key once lived there) */
    KEY_ENTER = 1 << 19,
    KEY_WEAPON5 = 1 << 20,
    KEY_WEAPON6 = 1 << 21,
    KEY_WEAPON7 = 1 << 22,
    KEY_MAP = 1 << 23,
};

/* Weapon number (1–7) -> its snapshot bit; index 0 is unused. */
static const uint64_t weaponBits[8] = {
    0, KEY_WEAPON1, KEY_WEAPON2, KEY_WEAPON3,
    KEY_WEAPON4, KEY_WEAPON5, KEY_WEAPON6, KEY_WEAPON7};
#define WEAPON_MASK                                                           \
    (KEY_WEAPON1 | KEY_WEAPON2 | KEY_WEAPON3 | KEY_WEAPON4 | KEY_WEAPON5 |    \
     KEY_WEAPON6 | KEY_WEAPON7)

/* Remember a weapon key going down. A full queue drops the press: eight
 * unread weapon presses means Lean has not polled in a very long time, and
 * the newest ones are the stale ones by then. */
static void weapon_push(int w) {
    if (w < 1 || w > 7 || weaponQueueCount == WEAPON_QUEUE_LEN)
        return;
    weaponQueue[(weaponQueueHead + weaponQueueCount) % WEAPON_QUEUE_LEN] =
        (uint8_t)w;
    weaponQueueCount++;
}

/* The snapshot bit a scancode drives, or 0 if the key is unbound. Used both
 * to route key events and to rebuild `heldKeys` from `scanHeld` each poll. */
static uint64_t key_bit(SDL_Scancode sc) {
    switch (sc) {
    case SDL_SCANCODE_W:
    case SDL_SCANCODE_UP:      return KEY_FORWARD;
    case SDL_SCANCODE_S:
    case SDL_SCANCODE_DOWN:    return KEY_BACK;
    case SDL_SCANCODE_A:       return KEY_STRAFE_LEFT;
    case SDL_SCANCODE_D:       return KEY_STRAFE_RIGHT;
    case SDL_SCANCODE_LEFT:    return KEY_TURN_LEFT;
    case SDL_SCANCODE_RIGHT:   return KEY_TURN_RIGHT;
    case SDL_SCANCODE_LSHIFT:
    case SDL_SCANCODE_RSHIFT:  return KEY_RUN;
    case SDL_SCANCODE_SPACE:   return KEY_USE;
    case SDL_SCANCODE_LCTRL:
    case SDL_SCANCODE_RCTRL:   return KEY_FIRE;
    case SDL_SCANCODE_1:       return KEY_WEAPON1;
    case SDL_SCANCODE_2:       return KEY_WEAPON2;
    case SDL_SCANCODE_3:       return KEY_WEAPON3;
    case SDL_SCANCODE_4:       return KEY_WEAPON4;
    case SDL_SCANCODE_5:       return KEY_WEAPON5;
    case SDL_SCANCODE_6:       return KEY_WEAPON6;
    case SDL_SCANCODE_7:       return KEY_WEAPON7;
    case SDL_SCANCODE_ESCAPE:  return KEY_PAUSE;
    case SDL_SCANCODE_F5:      return KEY_SAVE;
    case SDL_SCANCODE_F9:      return KEY_LOAD;
    case SDL_SCANCODE_RETURN:
    case SDL_SCANCODE_KP_ENTER: return KEY_ENTER;
    case SDL_SCANCODE_TAB:      return KEY_MAP;
    default:                    return 0;
    }
}

/* The oldest unreported weapon press, or 0 if there is none. */
static int weapon_pop(void) {
    if (weaponQueueCount == 0)
        return 0;
    int w = weaponQueue[weaponQueueHead];
    weaponQueueHead = (weaponQueueHead + 1) % WEAPON_QUEUE_LEN;
    weaponQueueCount--;
    return w;
}

LEAN_EXPORT lean_obj_res dill_poll(lean_obj_arg world) {
    (void)world;
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        switch (e.type) {
        case SDL_EVENT_QUIT:
            quitRequested = true;
            break;
        case SDL_EVENT_KEY_DOWN:
        case SDL_EVENT_KEY_UP: {
            bool down = (e.type == SDL_EVENT_KEY_DOWN);
            if (down && !e.key.repeat && typedCount < 8) {
                SDL_Scancode sc = e.key.scancode;
                char c = 0;
                if (sc >= SDL_SCANCODE_A && sc <= SDL_SCANCODE_Z)
                    c = (char)('a' + (sc - SDL_SCANCODE_A));
                else if (sc == SDL_SCANCODE_0)
                    c = '0';
                else if (sc >= SDL_SCANCODE_1 && sc <= SDL_SCANCODE_9)
                    c = (char)('1' + (sc - SDL_SCANCODE_1));
                if (c) {
                    typedQueue |= ((uint64_t)(uint8_t)c) << (8 * typedCount);
                    typedCount++;
                }
            }
            uint64_t bit = key_bit(e.key.scancode);
            if (bit) /* bound keys only; scancode < SDL_SCANCODE_COUNT then */
                scanHeld[e.key.scancode] = down;
            if (down) {
                /* Weapon presses go to the queue instead of `tappedKeys`,
                 * which cannot carry more than one of them per poll. Key
                 * auto-repeat is not a new press — a held weapon key would
                 * flood the 8-deep queue and crowd out real presses (a held
                 * key already reports every poll through `heldKeys`, so
                 * nothing is lost by skipping the repeat). Repeats are
                 * harmless for the plain bits: they OR into `tappedKeys`
                 * while the key is held anyway. */
                if (bit & WEAPON_MASK) {
                    if (!e.key.repeat)
                        for (int w = 1; w <= 7; w++)
                            if (bit == weaponBits[w]) { weapon_push(w); break; }
                } else {
                    tappedKeys |= bit;
                }
            }
            break;
        }
        case SDL_EVENT_MOUSE_MOTION:
            mouseDx += e.motion.xrel;
            break;
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
        case SDL_EVENT_MOUSE_BUTTON_UP:
            if (e.button.button == SDL_BUTTON_LEFT) {
                mouseFireHeld = (e.type == SDL_EVENT_MOUSE_BUTTON_DOWN);
                if (mouseFireHeld)
                    tappedKeys |= KEY_FIRE;
            }
            break;
        default:
            break;
        }
    }
    /* Rebuild the held mask from the physical sources (see `scanHeld`), so
     * aliased bindings OR together instead of overwriting a shared bit. */
    heldKeys = 0;
    for (int sc = 0; sc < SDL_SCANCODE_COUNT; sc++)
        if (scanHeld[sc]) heldKeys |= key_bit((SDL_Scancode)sc);
    if (mouseFireHeld) heldKeys |= KEY_FIRE;
    /* clamp while still a float: casting a float beyond int32 range is UB */
    float fdx = mouseDx;
    if (fdx < -32768.0f) fdx = -32768.0f;
    if (fdx > 32767.0f) fdx = 32767.0f;
    int32_t dx = (int32_t)fdx;
    mouseDx = 0.0f;
    uint64_t keys = heldKeys | tappedKeys;
    /* Exactly one weapon per snapshot: the oldest queued press, so each one
     * gets a poll of its own, and otherwise whatever is still held down. A
     * key that is both queued and held reports the same weapon either way. */
    int queued = weapon_pop();
    uint64_t weapon = queued ? weaponBits[queued] : (keys & WEAPON_MASK);
    uint64_t snapshot = (keys & ~(uint64_t)WEAPON_MASK) | weapon |
                        (quitRequested ? FLAG_QUIT : 0) |
                        ((uint64_t)(uint16_t)(int16_t)dx << 48);
    tappedKeys = 0; /* reported once; `heldKeys` carries it from here */
    return lean_io_result_mk_ok(lean_box_uint64(snapshot));
}

#ifndef __APPLE__
/* Nearest-neighbor resample of a `srcW`x`srcH` RGBA buffer to `W`x`H`.
 * Frees `src`; returns a fresh `W`x`H`x4 buffer (NULL on OOM). */
static uint8_t *resample_rgba(uint8_t *src, int srcW, int srcH, int W, int H) {
    uint8_t *out = malloc((size_t)W * H * 4);
    if (!out) { free(src); return NULL; }
    for (int y = 0; y < H; y++) {
        int sy = (int)((int64_t)y * srcH / H);
        for (int x = 0; x < W; x++) {
            int sx = (int)((int64_t)x * srcW / W);
            memcpy(out + ((size_t)y * W + x) * 4,
                   src + ((size_t)sy * srcW + sx) * 4, 4);
        }
    }
    free(src);
    return out;
}

/* Decode a PNG into a freshly malloc'd premultiplied-RGBA buffer, top row
 * first, via libpng. If `W`>0 the image is scaled to `W`x`H`; otherwise it
 * is decoded at its native pixel size and the size is written to `*outW`,
 * `*outH`. NULL on any failure. Alpha is premultiplied into RGB to match the
 * macOS path and the compositor's blend (`dst = src + dst*(255-a)/255`). */
static uint8_t *decode_png_rgba(const char *path, int W, int H,
                                int *outW, int *outH) {
    FILE *fp = fopen(path, "rb");
    if (!fp) return NULL;
    unsigned char sig[8];
    if (fread(sig, 1, 8, fp) != 8 || png_sig_cmp(sig, 0, 8)) {
        fclose(fp);
        return NULL;
    }
    png_structp png =
        png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png) { fclose(fp); return NULL; }
    png_infop info = png_create_info_struct(png);
    if (!info) {
        png_destroy_read_struct(&png, NULL, NULL);
        fclose(fp);
        return NULL;
    }
    /* `volatile`: both are written after the setjmp and read in the longjmp
     * handler below; without it those reads are undefined (C11 7.13.2.1). */
    uint8_t *volatile native = NULL;
    png_bytep *volatile rows = NULL;
    if (setjmp(png_jmpbuf(png))) { /* libpng longjmps here on any error */
        free(native);
        free(rows);
        png_destroy_read_struct(&png, &info, NULL);
        fclose(fp);
        return NULL;
    }
    png_init_io(png, fp);
    png_set_sig_bytes(png, 8);
    png_read_info(png, info);

    png_uint_32 nw = png_get_image_width(png, info);
    png_uint_32 nh = png_get_image_height(png, info);
    int bitDepth = png_get_bit_depth(png, info);
    int colorType = png_get_color_type(png, info);
    /* Normalize whatever the file is to 8-bit straight RGBA. */
    if (bitDepth == 16) png_set_strip_16(png);
    if (colorType == PNG_COLOR_TYPE_PALETTE) png_set_palette_to_rgb(png);
    if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8)
        png_set_expand_gray_1_2_4_to_8(png);
    if (png_get_valid(png, info, PNG_INFO_tRNS)) png_set_tRNS_to_alpha(png);
    if (colorType == PNG_COLOR_TYPE_GRAY ||
        colorType == PNG_COLOR_TYPE_GRAY_ALPHA)
        png_set_gray_to_rgb(png);
    if (colorType == PNG_COLOR_TYPE_RGB || colorType == PNG_COLOR_TYPE_GRAY ||
        colorType == PNG_COLOR_TYPE_PALETTE)
        png_set_add_alpha(png, 0xFF, PNG_FILLER_AFTER);
    png_read_update_info(png, info);

    native = malloc((size_t)nw * nh * 4);
    rows = malloc(sizeof(png_bytep) * nh);
    if (!native || !rows) png_error(png, "out of memory"); /* -> setjmp */
    for (png_uint_32 y = 0; y < nh; y++)
        rows[y] = native + (size_t)y * nw * 4;
    png_read_image(png, rows);
    free(rows);
    rows = NULL;
    png_destroy_read_struct(&png, &info, NULL);
    fclose(fp);

    /* Premultiply alpha into RGB, matching ImageIO's PremultipliedLast. */
    for (size_t i = 0; i < (size_t)nw * nh; i++) {
        uint8_t *p = native + i * 4;
        int a = p[3];
        p[0] = (uint8_t)(p[0] * a / 255);
        p[1] = (uint8_t)(p[1] * a / 255);
        p[2] = (uint8_t)(p[2] * a / 255);
    }

    if (W <= 0) {
        if (outW) *outW = (int)nw;
        if (outH) *outH = (int)nh;
        return native;
    }
    if (outW) *outW = W;
    if (outH) *outH = H;
    return resample_rgba(native, (int)nw, (int)nh, W, H);
}
#else
/* Decode a PNG into a freshly malloc'd premultiplied-RGBA buffer, top row
 * first, via macOS ImageIO. If `W`>0 the image is scaled to `W`x`H`;
 * otherwise it's decoded at its native pixel size and the size is written
 * to `*outW`,`*outH`. NULL on any failure. */
static uint8_t *decode_png_rgba(const char *path, int W, int H,
                                int *outW, int *outH) {
    CFStringRef s = CFStringCreateWithCString(NULL, path, kCFStringEncodingUTF8);
    if (!s) return NULL;
    CFURLRef url = CFURLCreateWithFileSystemPath(NULL, s, kCFURLPOSIXPathStyle,
                                                 false);
    CFRelease(s);
    if (!url) return NULL;
    CGImageSourceRef src = CGImageSourceCreateWithURL(url, NULL);
    CFRelease(url);
    if (!src) return NULL;
    CGImageRef img = CGImageSourceCreateImageAtIndex(src, 0, NULL);
    CFRelease(src);
    if (!img) return NULL;

    if (W <= 0) { W = (int)CGImageGetWidth(img); H = (int)CGImageGetHeight(img); }
    if (outW) *outW = W;
    if (outH) *outH = H;
    uint8_t *px = calloc((size_t)W * H, 4);
    if (!px) { CGImageRelease(img); return NULL; }
    CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
    CGContextRef ctx = CGBitmapContextCreate(
        px, W, H, 8, (size_t)W * 4, cs,
        kCGImageAlphaPremultipliedLast | kCGBitmapByteOrder32Big);
    CGColorSpaceRelease(cs);
    if (!ctx) { CGImageRelease(img); free(px); return NULL; }
    CGContextDrawImage(ctx, CGRectMake(0, 0, W, H), img);
    CGContextRelease(ctx);
    CGImageRelease(img);
    return px; /* row 0 = top, exactly what the compositor expects */
}
#endif /* __APPLE__ */

/* Re-decode the overlay at its native size and (re)allocate the swapchain-
 * sized host present buffer. Disables the overlay if anything fails. */
static void refresh_overlay(void) {
    if (!overlayActive || !device)
        return;
    int W = (int)swapchainExtent.width, H = (int)swapchainExtent.height;
    free(overlayRGBA);
    overlayRGBA = decode_png_rgba(overlayPath, 0, 0, &overlayW, &overlayH);
    if (!overlayRGBA) { overlayActive = false; return; }

    VkDeviceSize need = (VkDeviceSize)W * H * 4;
    if (need > presentCap) {
        /* Only ever reached with the device idle: dill_init has no frames in
         * flight yet, recreate_swapchain waits idle first, and
         * dill_set_overlay zeroes presentCap and then waits idle. */
        if (presentBuf) {
            vkUnmapMemory(device, presentMem);
            vkDestroyBuffer(device, presentBuf, NULL);
            vkFreeMemory(device, presentMem, NULL);
            presentBuf = VK_NULL_HANDLE;
            presentMem = VK_NULL_HANDLE;
            presentPtr = NULL;
        }
        presentCap = 0;
        VkBufferCreateInfo bci = {
            .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
            .size = need,
            .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        };
        if (vkCreateBuffer(device, &bci, NULL, &presentBuf) != VK_SUCCESS) {
            /* output handle is undefined on failure; keep state consistent */
            presentBuf = VK_NULL_HANDLE;
            overlayActive = false;
            return;
        }
        VkMemoryRequirements mr;
        vkGetBufferMemoryRequirements(device, presentBuf, &mr);
        /* find_memory_type's UINT32_MAX sentinel would be undefined
         * behaviour inside vkAllocateMemory (out-of-range memoryTypeIndex
         * is not a checkable error); the overlay is optional, so a device
         * with no host-visible type just goes without it. */
        uint32_t presentType = find_memory_type(
            mr.memoryTypeBits, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                                   VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
        if (presentType == UINT32_MAX) {
            vkDestroyBuffer(device, presentBuf, NULL);
            presentBuf = VK_NULL_HANDLE;
            overlayActive = false;
            return;
        }
        VkMemoryAllocateInfo mai = {
            .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
            .allocationSize = mr.size,
            .memoryTypeIndex = presentType,
        };
        if (vkAllocateMemory(device, &mai, NULL, &presentMem) != VK_SUCCESS) {
            /* free/unmap only what was actually created */
            vkDestroyBuffer(device, presentBuf, NULL);
            presentBuf = VK_NULL_HANDLE;
            presentMem = VK_NULL_HANDLE;
            overlayActive = false;
            return;
        }
        if (vkBindBufferMemory(device, presentBuf, presentMem, 0) != VK_SUCCESS ||
            vkMapMemory(device, presentMem, 0, mr.size, 0, &presentPtr) !=
                VK_SUCCESS) {
            vkDestroyBuffer(device, presentBuf, NULL);
            vkFreeMemory(device, presentMem, NULL);
            presentBuf = VK_NULL_HANDLE;
            presentMem = VK_NULL_HANDLE;
            presentPtr = NULL;
            overlayActive = false;
            return;
        }
        presentCap = need;
    }
}

/* The game's letterbox/cover placement inside the window, shared by the
 * GPU blit (no overlay) and the CPU compositor (overlay). */
static void present_rect(int32_t winW, int32_t winH, int32_t *srcX0,
                         int32_t *srcX1, int32_t *srcY0, int32_t *srcY1,
                         int32_t *ox, int32_t *oy, int32_t *dstW,
                         int32_t *dstH) {
    *srcX0 = 0; *srcX1 = FRAME_W; *srcY0 = 0; *srcY1 = FRAME_H;
    *ox = 0; *oy = 0; *dstW = winW; *dstH = winH;
    if (presentContain) {
        /* letterbox the whole 16:9 frame so nothing is cropped (menus) */
        int32_t aspW = FRAME_W, aspH = FRAME_H * 6 / 5;
        if (winW * aspH > winH * aspW) *dstW = winH * aspW / aspH;
        else *dstH = winW * aspH / aspW;
        *ox = (winW - *dstW) / 2;
        *oy = (winH - *dstH) / 2;
    } else if (classicAspect) {
        *srcX0 = (FRAME_W - CLASSIC_W) / 2;
        *srcX1 = *srcX0 + CLASSIC_W;
        int32_t aspW = CLASSIC_W, aspH = FRAME_H * 6 / 5;
        if (winW * aspH > winH * aspW) *dstW = winH * aspW / aspH;
        else *dstH = winW * aspH / aspW;
        *ox = (winW - *dstW) / 2;
        *oy = (winH - *dstH) / 2;
    } else {
        double winAspect = (double)winW / (double)winH;
        double effW = FRAME_W, effH = FRAME_H * 1.2;
        if (winAspect < effW / effH) {
            int32_t srcW = (int32_t)(effH * winAspect + 0.5);
            *srcX0 = (FRAME_W - srcW) / 2;
            *srcX1 = *srcX0 + srcW;
        } else {
            int32_t srcH = (int32_t)(effW / winAspect / 1.2 + 0.5);
            *srcY0 = (FRAME_H - srcH) / 2;
            *srcY1 = *srcY0 + srcH;
        }
    }
    /* A degenerate window (a sliver during a live resize, say) can round a
     * letterboxed dimension down to 0, and a zero-area blit region is a
     * validation error — and a divide-by-zero in the CPU compositor. */
    if (*dstW < 1) *dstW = 1;
    if (*dstH < 1) *dstH = 1;
    /* The cover path's *source* crop can likewise round to 0 at an extreme
     * window aspect ratio, and a zero-area blit source is just as invalid
     * as a zero-area destination. */
    if (*srcX1 <= *srcX0) *srcX1 = *srcX0 + 1;
    if (*srcY1 <= *srcY0) *srcY1 = *srcY0 + 1;
}

/* Placement of the game and overlay within one output row, shared by both
 * the parallel (macOS/GCD) and serial (Linux) compositors. */
typedef struct {
    const uint8_t *game;
    uint8_t *out;
    int W;                 /* swapchain width */
    int32_t srcX0, srcX1, srcY0, srcY1; /* game crop rectangle */
    int ox, oy, dstW, dstH;             /* game placement in the window */
    int offX, offY;        /* overlay anchor (top-left, 1:1) */
    bool bgra;             /* swapchain channel order */
} CompositeCtx;

/* Composite one output row `y`: the upscaled game pixel under alpha-blended
 * overlay, written in the swapchain's channel order. */
static void composite_row(const CompositeCtx *c, int y) {
    int inRowY = (y >= c->oy && y < c->oy + c->dstH);
    int gy = inRowY ? c->srcY0 + (y - c->oy) * (c->srcY1 - c->srcY0) / c->dstH
                    : 0;
    const uint8_t *grow = c->game + (size_t)gy * FRAME_W * 4;
    int ovy = y - c->offY;
    const uint8_t *orow = (ovy >= 0 && ovy < overlayH)
        ? overlayRGBA + (size_t)ovy * overlayW * 4 : NULL;
    uint8_t *drow = c->out + (size_t)y * c->W * 4;
    for (int x = 0; x < c->W; x++) {
        int r = 0, g = 0, b = 0;
        if (inRowY && x >= c->ox && x < c->ox + c->dstW) {
            int gx = c->srcX0 + (x - c->ox) * (c->srcX1 - c->srcX0) / c->dstW;
            const uint8_t *gp = grow + (size_t)gx * 4;
            r = gp[0]; g = gp[1]; b = gp[2];
        }
        if (orow) {
            int ovx = x - c->offX;
            if (ovx >= 0 && ovx < overlayW) {
                const uint8_t *op = orow + (size_t)ovx * 4;
                int a = op[3];
                if (a) {
                    int inv = 255 - a;
                    r = op[0] + r * inv / 255;
                    g = op[1] + g * inv / 255;
                    b = op[2] + b * inv / 255;
                }
            }
        }
        uint8_t *dp = drow + (size_t)x * 4;
        if (c->bgra) { dp[0] = b; dp[1] = g; dp[2] = r; }
        else         { dp[0] = r; dp[1] = g; dp[2] = b; }
        dp[3] = 255;
    }
}

/* Software-composite the (upscaled) game plus the native-res overlay into
 * the present buffer, in the swapchain's channel order. `game` is the
 * 426x200 RGBA framebuffer. */
static void composite_overlay(const uint8_t *game) {
    int W = (int)swapchainExtent.width, H = (int)swapchainExtent.height;
    CompositeCtx c = {
        .game = game, .out = presentPtr, .W = W,
        .bgra = (swapFormat == VK_FORMAT_B8G8R8A8_UNORM ||
                 swapFormat == VK_FORMAT_B8G8R8A8_SRGB),
        /* the overlay maps 1:1 to screen pixels (no scaling), top-left */
        .offX = 0, .offY = 0,
    };
    present_rect(W, H, &c.srcX0, &c.srcX1, &c.srcY0, &c.srcY1, &c.ox, &c.oy,
                 &c.dstW, &c.dstH);
    /* Rows are independent. A full-screen composite at native res costs
     * several ms on one thread, so macOS fans it across cores with GCD;
     * Linux runs it serially (no libdispatch dependency). */
#ifdef __APPLE__
    dispatch_apply(H, dispatch_get_global_queue(QOS_CLASS_USER_INTERACTIVE, 0),
                   ^(size_t yy) { composite_row(&c, (int)yy); });
#else
    for (int y = 0; y < H; y++) composite_row(&c, y);
#endif
}

/* Enable/replace the overlay from a PNG path; returns whether it loaded. */
LEAN_EXPORT lean_obj_res dill_set_overlay(b_lean_obj_arg path,
                                          lean_obj_arg world) {
    (void)world;
    strncpy(overlayPath, lean_string_cstr(path), sizeof overlayPath - 1);
    overlayPath[sizeof overlayPath - 1] = 0;
    overlayActive = true;
    presentCap = 0; /* force a fresh buffer sized to the swapchain */
    if (presentBuf) {
        /* the last submitted present may still be copying out of this
         * buffer; destroying it mid-flight is undefined behaviour */
        vkDeviceWaitIdle(device);
        vkUnmapMemory(device, presentMem);
        vkDestroyBuffer(device, presentBuf, NULL);
        vkFreeMemory(device, presentMem, NULL);
        presentBuf = VK_NULL_HANDLE;
        presentMem = VK_NULL_HANDLE;
        presentPtr = NULL;
    }
    refresh_overlay();
    return lean_io_result_mk_ok(lean_box_uint32(overlayActive ? 1 : 0));
}

/* Decode a PNG scaled to `w`x`h`, returned as Option (premult-RGBA
 * ByteArray, w*h*4 bytes). Used to import an image into a palette Picture
 * (e.g. a custom title logo). None if missing or unreadable. */
/* No image may be asked for at more than this on a side. The bound exists
 * because the size crosses the FFI as two uint32s and is then used as `int`
 * and multiplied out: `(int)w` is implementation-defined above INT_MAX, and
 * `(size_t)w * h * 4` overflows for large pairs — either of which turns a
 * bad argument into a wild allocation. 16384 is past any display this will
 * ever composite over. */
#define MAX_DECODE_DIM 16384

LEAN_EXPORT lean_obj_res dill_decode_png(b_lean_obj_arg path, uint32_t w,
                                         uint32_t h, lean_obj_arg world) {
    (void)world;
    /* A zero dimension is not "decode at native size" here — that is what
     * `decode_png_rgba` would do with it, and it would then hand back a
     * full-size image while the `w*h*4` below sized the ByteArray at 0. The
     * caller asked for something impossible; say none. */
    if (w == 0 || h == 0 || w > MAX_DECODE_DIM || h > MAX_DECODE_DIM)
        return lean_io_result_mk_ok(lean_box(0));
    uint8_t *px = decode_png_rgba(lean_string_cstr(path), (int)w, (int)h,
                                  NULL, NULL);
    /* Option.none is a fieldless ctor: the canonical form is the scalar
     * lean_box(0), not a heap-allocated zero-field ctor. */
    if (!px) return lean_io_result_mk_ok(lean_box(0));
    size_t n = (size_t)w * h * 4;
    lean_object *arr = lean_alloc_sarray(1, n, n);
    memcpy(lean_sarray_cptr(arr), px, n);
    free(px);
    lean_object *some = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(some, 0, arr);
    return lean_io_result_mk_ok(some);
}

LEAN_EXPORT lean_obj_res dill_aspect(uint32_t classic, lean_obj_arg world) {
    (void)world;
    classicAspect = classic != 0;
    return io_ok();
}

/* Letterbox the whole frame (menus/title) instead of edge-cropping cover. */
LEAN_EXPORT lean_obj_res dill_fit(uint32_t on, lean_obj_arg world) {
    (void)world;
    presentContain = on != 0;
    return io_ok();
}

LEAN_EXPORT lean_obj_res dill_sound_load(uint32_t idx, uint32_t rate,
                                         b_lean_obj_arg bytes,
                                         lean_obj_arg world) {
    (void)world;
    if (idx >= MAX_SOUNDS)
        return io_ok();
    size_t n = lean_sarray_size(bytes);
    uint8_t *copy = malloc(n);
    if (!copy)
        return io_ok();
    memcpy(copy, lean_sarray_cptr(bytes), n);
    free(soundClips[idx].data);
    soundClips[idx].data = copy;
    soundClips[idx].len = (int)n;
    soundClips[idx].rate = (int)rate;
    return io_ok();
}

LEAN_EXPORT lean_obj_res dill_sound_play(uint32_t idx, double gain,
                                        lean_obj_arg world) {
    (void)world;
    if (!audioReady || idx >= MAX_SOUNDS || !soundClips[idx].data)
        return io_ok();
    int ch;
    if (idx >= SAW_SFX_FIRST && idx <= SAW_SFX_LAST) {
        /* the chainsaw's reserved channel: clearing below halts the last saw
         * voice, so the idle putter and the cutting growl never overlap */
        ch = SAW_CHANNEL;
    } else {
        /* world sound: prefer an idle channel (never the reserved saw one),
         * otherwise steal round-robin among the remaining channels */
        static unsigned nextSteal; /* unsigned: wraparound is defined */
        ch = -1;
        for (int i = 0; i < NUM_CHANNELS; i++) {
            if (i == SAW_CHANNEL)
                continue;
            if (SDL_GetAudioStreamAvailable(audioChannels[i]) == 0) {
                ch = i;
                break;
            }
        }
        if (ch < 0) {
            ch = nextSteal++ % (NUM_CHANNELS - 1);
            if (ch >= SAW_CHANNEL)
                ch += 1; /* map 0..N-2 onto the non-saw channels */
        }
    }
    SDL_AudioSpec spec = {SDL_AUDIO_U8, 1, soundClips[idx].rate};
    SDL_ClearAudioStream(audioChannels[ch]);
    SDL_SetAudioStreamFormat(audioChannels[ch], &spec, NULL);
    SDL_SetAudioStreamGain(audioChannels[ch], (float)gain);
    SDL_PutAudioStreamData(audioChannels[ch], soundClips[idx].data,
                           soundClips[idx].len);
    return io_ok();
}

static void music_stop_now(void) {
#ifdef __APPLE__
    if (musicPlayer) {
        MusicPlayerStop(musicPlayer);
        DisposeMusicPlayer(musicPlayer);
        musicPlayer = NULL;
    }
    if (musicSeq) {
        DisposeMusicSequence(musicSeq);
        musicSeq = NULL;
    }
#endif
}

#ifdef __APPLE__
LEAN_EXPORT lean_obj_res dill_music_play(b_lean_obj_arg midi,
                                         lean_obj_arg world) {
    (void)world;
    music_stop_now();
    CFDataRef data = CFDataCreate(NULL, lean_sarray_cptr(midi),
                                  (CFIndex)lean_sarray_size(midi));
    if (!data)
        return io_ok();
    bool ok = NewMusicSequence(&musicSeq) == noErr &&
              MusicSequenceFileLoadData(musicSeq, data,
                                        kMusicSequenceFile_MIDIType,
                                        0) == noErr;
    CFRelease(data);
    if (!ok) {
        music_stop_now();
        return io_ok();
    }
    /* loop every track forever */
    UInt32 ntracks = 0;
    MusicSequenceGetTrackCount(musicSeq, &ntracks);
    for (UInt32 i = 0; i < ntracks; i++) {
        MusicTrack track;
        if (MusicSequenceGetIndTrack(musicSeq, i, &track) != noErr)
            continue;
        MusicTimeStamp len = 0;
        UInt32 sz = sizeof len;
        MusicTrackGetProperty(track, kSequenceTrackProperty_TrackLength,
                              &len, &sz);
        MusicTrackLoopInfo loop = {len, 0};
        MusicTrackSetProperty(track, kSequenceTrackProperty_LoopInfo, &loop,
                              sizeof loop);
    }
    if (NewMusicPlayer(&musicPlayer) != noErr ||
        MusicPlayerSetSequence(musicPlayer, musicSeq) != noErr ||
        MusicPlayerStart(musicPlayer) != noErr)
        music_stop_now();
    return io_ok();
}
#else
/* Linux has no built-in General MIDI synth; music is silently skipped.
 * Sound effects (SDL audio) are unaffected. */
LEAN_EXPORT lean_obj_res dill_music_play(b_lean_obj_arg midi,
                                         lean_obj_arg world) {
    (void)world;
    (void)midi;
    return io_ok();
}
#endif /* __APPLE__ */

LEAN_EXPORT lean_obj_res dill_music_stop(lean_obj_arg world) {
    (void)world;
    music_stop_now();
    return io_ok();
}

LEAN_EXPORT lean_obj_res dill_typed(lean_obj_arg world) {
    (void)world;
    uint64_t q = typedQueue;
    typedQueue = 0;
    typedCount = 0;
    return lean_io_result_mk_ok(lean_box_uint64(q));
}

LEAN_EXPORT lean_obj_res dill_ticks(lean_obj_arg world) {
    (void)world;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)SDL_GetTicks()));
}

/* Tear everything down. Two properties this must hold, because Lean's
 * top-level error handler calls it for *any* failed command:
 *
 *  - It tears down only what was actually brought up. `dill_init` returns
 *    on the first Vulkan call that fails, so it can leave a device with no
 *    staging buffer behind it — and `vkUnmapMemory` on a null memory handle
 *    is undefined behaviour, unlike the `vkDestroy*` family, which accepts
 *    VK_NULL_HANDLE by spec. Hence the guards on the two unmaps.
 *  - It is idempotent: every handle is cleared as it is released, so the
 *    ordinary `Shell.shutdown` at the end of a command followed by the
 *    error handler's is a no-op the second time round. Non-graphical
 *    commands (`info`, `map`, `ppm`, …) reach it having initialised
 *    nothing at all, which is the same path with every handle already null.
 */
LEAN_EXPORT lean_obj_res dill_shutdown(lean_obj_arg world) {
    (void)world;
    music_stop_now();
    for (int i = 0; i < NUM_CHANNELS; i++)
        if (audioChannels[i]) {
            SDL_DestroyAudioStream(audioChannels[i]);
            audioChannels[i] = NULL;
        }
    audioReady = false;
    for (int i = 0; i < MAX_SOUNDS; i++) {
        free(soundClips[i].data);
        soundClips[i].data = NULL;
    }
    free(overlayRGBA);
    overlayRGBA = NULL;
    overlayActive = false;
    /* input latches, so a re-init does not inherit the old run's presses */
    weaponQueueHead = weaponQueueCount = 0;
    memset(scanHeld, 0, sizeof scanHeld);
    mouseFireHeld = false;
    tappedKeys = heldKeys = 0;
    mouseDx = 0.0f;
    quitRequested = false;
    typedQueue = 0;
    typedCount = 0;
    if (device) {
        /* Waiting idle is what makes a pending `acquireSem` safe to destroy
         * below when a frame was abandoned between acquire and submit — see
         * `presentBroken`. Clear the flag last: a fresh `dill_init` starts a
         * new device with none of this frame's state. */
        presentBroken = false;
        vkDeviceWaitIdle(device);
        for (int i = 0; i < MAX_SWAPCHAIN_IMAGES; i++) {
            /* vkDestroySemaphore accepts VK_NULL_HANDLE by spec, so the
             * slots past the image count need no guard */
            vkDestroySemaphore(device, renderSems[i], NULL);
            renderSems[i] = VK_NULL_HANDLE;
        }
        vkDestroySemaphore(device, acquireSem, NULL);
        vkDestroyFence(device, inFlight, NULL);
        vkDestroyCommandPool(device, cmdPool, NULL);
        vkDestroyImage(device, frameImage, NULL);
        vkFreeMemory(device, frameImageMem, NULL);
        acquireSem = VK_NULL_HANDLE;
        inFlight = VK_NULL_HANDLE;
        cmdPool = VK_NULL_HANDLE;
        frameImage = VK_NULL_HANDLE;
        frameImageMem = VK_NULL_HANDLE;
        if (stagingMem) {
            if (stagingPtr) {
                vkUnmapMemory(device, stagingMem);
                stagingPtr = NULL;
            }
            vkDestroyBuffer(device, staging, NULL);
            vkFreeMemory(device, stagingMem, NULL);
            staging = VK_NULL_HANDLE;
            stagingMem = VK_NULL_HANDLE;
        }
        if (presentMem) {
            if (presentPtr) {
                vkUnmapMemory(device, presentMem);
                presentPtr = NULL;
            }
            vkDestroyBuffer(device, presentBuf, NULL);
            vkFreeMemory(device, presentMem, NULL);
            presentBuf = VK_NULL_HANDLE;
            presentMem = VK_NULL_HANDLE;
            presentCap = 0;
        }
        vkDestroySwapchainKHR(device, swapchain, NULL);
        swapchain = VK_NULL_HANDLE;
        vkDestroyDevice(device, NULL);
        device = VK_NULL_HANDLE;
    }
    if (instance) {
        vkDestroySurfaceKHR(instance, surface, NULL);
        vkDestroyInstance(instance, NULL);
        surface = VK_NULL_HANDLE;
        instance = VK_NULL_HANDLE;
    }
    if (window) {
        SDL_DestroyWindow(window);
        window = NULL;
    }
    SDL_Quit();
    return io_ok();
}
