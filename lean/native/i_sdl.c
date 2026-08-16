/* Thin SDL2 backend for `lake exe doom`. Original wrapper; not copied from oracle. */

#define SDL_MAIN_HANDLED
#if defined(__has_include)
#  if __has_include(<SDL2/SDL.h>)
#    include <SDL2/SDL.h>
#  else
#    include <SDL.h>
#  endif
#else
#  include <SDL.h>
#endif

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#define SCREENWIDTH 320
#define SCREENHEIGHT 200
#define SCALE 2
#define TICRATE 35
#define EVQ_CAP 64

#define KEY_RIGHTARROW 0xae
#define KEY_LEFTARROW 0xac
#define KEY_UPARROW 0xad
#define KEY_DOWNARROW 0xaf
#define KEY_ESCAPE 27
#define KEY_RSHIFT (0x80 + 0x36)
#define KEY_RCTRL (0x80 + 0x1d)
#define KEY_RALT (0x80 + 0x38)

#define EV_KEYDOWN 1u
#define EV_KEYUP 2u
#define EV_QUIT 3u

static SDL_Window *window;
static SDL_Renderer *renderer;
static SDL_Texture *texture;
static uint32_t basetime;
static int initialized;

static uint32_t evq[EVQ_CAP];
static int evq_head;
static int evq_tail;

static void evq_push(uint32_t packed) {
    int next = (evq_tail + 1) % EVQ_CAP;
    if (next == evq_head)
        return;
    evq[evq_tail] = packed;
    evq_tail = next;
}

static uint32_t evq_pop(void) {
    if (evq_head == evq_tail)
        return 0;
    uint32_t v = evq[evq_head];
    evq_head = (evq_head + 1) % EVQ_CAP;
    return v;
}

static int translate_scancode(SDL_Scancode sc) {
    switch (sc) {
    case SDL_SCANCODE_RIGHT:
        return KEY_RIGHTARROW;
    case SDL_SCANCODE_LEFT:
        return KEY_LEFTARROW;
    case SDL_SCANCODE_UP:
        return KEY_UPARROW;
    case SDL_SCANCODE_DOWN:
        return KEY_DOWNARROW;
    case SDL_SCANCODE_SPACE:
        return ' ';
    case SDL_SCANCODE_COMMA:
        return ',';
    case SDL_SCANCODE_PERIOD:
        return '.';
    case SDL_SCANCODE_LCTRL:
    case SDL_SCANCODE_RCTRL:
        return KEY_RCTRL;
    case SDL_SCANCODE_LSHIFT:
    case SDL_SCANCODE_RSHIFT:
        return KEY_RSHIFT;
    case SDL_SCANCODE_LALT:
    case SDL_SCANCODE_RALT:
        return KEY_RALT;
    case SDL_SCANCODE_ESCAPE:
        return KEY_ESCAPE;
    default:
        return 0;
    }
}

uint32_t doom_i_get_time(void) {
    uint32_t ticks = SDL_GetTicks();
    if (basetime == 0)
        basetime = ticks;
    ticks -= basetime;
    return (ticks * TICRATE) / 1000;
}

void doom_i_sleep(uint32_t ms) {
    SDL_Delay((Uint32)ms);
}

int doom_i_init_graphics(void) {
    SDL_SetMainReady();
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) != 0)
        return 1;
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "0");
    window = SDL_CreateWindow(
        "DOOM",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        SCREENWIDTH * SCALE,
        SCREENHEIGHT * SCALE,
        SDL_WINDOW_ALLOW_HIGHDPI);
    if (window == NULL)
        return 2;
    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
    if (renderer == NULL)
        return 3;
    SDL_RenderSetLogicalSize(renderer, SCREENWIDTH, SCREENHEIGHT);
    SDL_RenderSetIntegerScale(renderer, SDL_TRUE);
    texture = SDL_CreateTexture(
        renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        SCREENWIDTH,
        SCREENHEIGHT);
    if (texture == NULL)
        return 4;
    initialized = 1;
    return 0;
}

void doom_i_shutdown(void) {
    if (texture != NULL) {
        SDL_DestroyTexture(texture);
        texture = NULL;
    }
    if (renderer != NULL) {
        SDL_DestroyRenderer(renderer);
        renderer = NULL;
    }
    if (window != NULL) {
        SDL_DestroyWindow(window);
        window = NULL;
    }
    if (initialized) {
        SDL_Quit();
        initialized = 0;
    }
}

void doom_i_start_tic(void) {
    SDL_Event e;
    if (!initialized)
        return;
    SDL_PumpEvents();
    while (SDL_PollEvent(&e)) {
        switch (e.type) {
        case SDL_KEYDOWN:
        case SDL_KEYUP: {
            int key = translate_scancode(e.key.keysym.scancode);
            if (key == 0)
                break;
            uint32_t kind = (e.type == SDL_KEYDOWN) ? EV_KEYDOWN : EV_KEYUP;
            evq_push((kind << 16) | ((uint32_t)key & 0xffffu));
            break;
        }
        case SDL_QUIT:
            evq_push(EV_QUIT << 16);
            break;
        default:
            break;
        }
    }
}

uint32_t doom_i_poll_event(void) {
    return evq_pop();
}

void doom_i_finish_update(const uint8_t *fb, size_t fb_len, const uint8_t *pal, size_t pal_len) {
    uint32_t *pixels;
    int pitch;
    int y;
    int x;
    if (!initialized || fb == NULL || pal == NULL)
        return;
    if (fb_len < (size_t)(SCREENWIDTH * SCREENHEIGHT) || pal_len < 768)
        return;
    if (SDL_LockTexture(texture, NULL, (void **)&pixels, &pitch) != 0)
        return;
    for (y = 0; y < SCREENHEIGHT; y++) {
        uint32_t *row = (uint32_t *)((uint8_t *)pixels + y * pitch);
        const uint8_t *src = fb + y * SCREENWIDTH;
        for (x = 0; x < SCREENWIDTH; x++) {
            unsigned idx = src[x];
            uint8_t r = pal[idx * 3u];
            uint8_t g = pal[idx * 3u + 1u];
            uint8_t b = pal[idx * 3u + 2u];
            row[x] = 0xff000000u | ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
        }
    }
    SDL_UnlockTexture(texture);
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);
}
