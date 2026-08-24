#ifndef DOOM_I_SDL_H
#define DOOM_I_SDL_H

#include <stdint.h>
#include <stddef.h>

int doom_i_init_graphics(void);
void doom_i_shutdown(void);
uint32_t doom_i_get_time(void);
void doom_i_sleep(uint32_t ms);
void doom_i_start_tic(void);
uint32_t doom_i_poll_event(void);
void doom_i_finish_update(const uint8_t *fb, size_t fb_len, const uint8_t *pal, size_t pal_len);

#endif
