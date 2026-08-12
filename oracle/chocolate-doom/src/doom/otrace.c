// Trace harness writer for Doom Lean verification (TRACE.md v1).

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SDL.h"

#include "doomdef.h"
#include "doomstat.h"
#include "d_player.h"
#include "d_think.h"
#include "i_sound.h"
#include "i_system.h"
#include "i_video.h"
#include "info.h"
#include "m_argv.h"
#include "m_misc.h"
#include "p_local.h"
#include "p_mobj.h"
#include "p_spec.h"
#include "r_state.h"

#include "otrace.h"

/* Declared in p_lights.c; not exported via p_spec.h in upstream. */
void T_FireFlicker(fireflicker_t *flick);

#define OTRACE_FNV_OFFSET 0xCBF29CE484222325ULL
#define OTRACE_FNV_PRIME  0x00000100000001B3ULL
#define OTRACE_NO_MO      0xFFFFFFFFu
#define OTRACE_MAX_FBTICS 256

enum
{
    THF_REMOVED = 0,
    THF_MOBJ = 1,
    THF_VERTICALDOOR = 2,
    THF_MOVECEILING = 3,
    THF_MOVEFLOOR = 4,
    THF_PLATRAISE = 5,
    THF_LIGHTFLASH = 6,
    THF_STROBEFLASH = 7,
    THF_GLOW = 8,
    THF_FIREFLICKER = 9,
    THF_NULL = 10
};

static boolean otrace_wanted;
static boolean otrace_active;
static boolean otrace_shutting_down;

static FILE *trc_fp;
static FILE *dig_fp;

static int maxtics; /* 0 = unlimited */
static int records_emitted;

static int fbtics[OTRACE_MAX_FBTICS];
static boolean fbtics_dumped[OTRACE_MAX_FBTICS];
static int fbtics_count;
static char *fbdir;

static byte raw_palette[768];
static boolean raw_palette_valid;

static unsigned char *rec_buf;
static size_t rec_len;
static size_t rec_cap;

/* Random table indices live in m_random.c but are not exported in the header. */
extern int rndindex;
extern int prndindex;

extern char *video_driver;

static void OTrace_HarnessError(const char *msg) NORETURN;
static void OTrace_HarnessError(const char *msg)
{
    fprintf(stderr, "otrace: %s\n", msg);
    OTrace_Shutdown();
    exit(2);
}

static uint64_t OTrace_FNV1a64(const unsigned char *data, size_t len)
{
    uint64_t hash = OTRACE_FNV_OFFSET;
    size_t i;

    for (i = 0; i < len; ++i)
    {
        hash ^= data[i];
        hash *= OTRACE_FNV_PRIME;
    }

    return hash;
}

static void OTrace_RecReset(void)
{
    rec_len = 0;
}

static void OTrace_RecEnsure(size_t need)
{
    size_t new_cap;
    unsigned char *nbuf;

    if (rec_len + need <= rec_cap)
    {
        return;
    }

    new_cap = rec_cap ? rec_cap : 4096;
    while (rec_len + need > new_cap)
    {
        new_cap *= 2;
    }

    nbuf = realloc(rec_buf, new_cap);
    if (nbuf == NULL)
    {
        OTrace_HarnessError("out of memory growing record buffer");
    }

    rec_buf = nbuf;
    rec_cap = new_cap;
}

static void OTrace_PutU32(uint32_t v)
{
    OTrace_RecEnsure(4);
    rec_buf[rec_len++] = (unsigned char)(v & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 8) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 16) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 24) & 0xffu);
}

static void OTrace_PutI32(int32_t v)
{
    OTrace_PutU32((uint32_t)v);
}

static void OTrace_PutU64(uint64_t v)
{
    OTrace_RecEnsure(8);
    rec_buf[rec_len++] = (unsigned char)(v & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 8) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 16) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 24) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 32) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 40) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 48) & 0xffu);
    rec_buf[rec_len++] = (unsigned char)((v >> 56) & 0xffu);
}

static void OTrace_WriteHeader(FILE *fp, const char *magic4)
{
    unsigned char hdr[16];

    hdr[0] = (unsigned char)magic4[0];
    hdr[1] = (unsigned char)magic4[1];
    hdr[2] = (unsigned char)magic4[2];
    hdr[3] = (unsigned char)magic4[3];
    hdr[4] = 1;
    hdr[5] = 0;
    hdr[6] = 0;
    hdr[7] = 0; /* version = 1 LE */
    hdr[8] = 0;
    hdr[9] = 0;
    hdr[10] = 0;
    hdr[11] = 0; /* flags */
    hdr[12] = 0;
    hdr[13] = 0;
    hdr[14] = 0;
    hdr[15] = 0; /* reserved */

    if (fwrite(hdr, 1, 16, fp) != 16)
    {
        OTrace_HarnessError("failed writing file header");
    }
}

static void OTrace_ParseFbtics(const char *arg)
{
    const char *p = arg;

    fbtics_count = 0;
    while (*p)
    {
        char *end;
        long v;

        while (*p == ',')
        {
            ++p;
        }
        if (*p == '\0')
        {
            break;
        }

        errno = 0;
        v = strtol(p, &end, 10);
        if (end == p || errno != 0 || v < 0)
        {
            OTrace_HarnessError("invalid -fbtics list");
        }
        if (fbtics_count >= OTRACE_MAX_FBTICS)
        {
            OTrace_HarnessError("too many -fbtics entries");
        }
        fbtics[fbtics_count] = (int)v;
        fbtics_dumped[fbtics_count] = false;
        fbtics_count++;
        p = end;
    }
}

static void OTrace_OnSetPalette(byte *palette)
{
    if (!otrace_active || palette == NULL)
    {
        return;
    }

    memcpy(raw_palette, palette, 768);
    raw_palette_valid = true;
}

static void OTrace_DumpFramebuffer(int gametic_just_run)
{
    int i;
    char path[4096];
    FILE *fp;
    uint64_t h;
    unsigned char digest_input[64000 + 768];

    if (!otrace_active || fbdir == NULL || fbtics_count == 0)
    {
        return;
    }
    if (I_VideoBuffer == NULL || !raw_palette_valid)
    {
        return;
    }

    for (i = 0; i < fbtics_count; ++i)
    {
        int x;

        if (fbtics_dumped[i] || fbtics[i] != gametic_just_run)
        {
            continue;
        }

        M_snprintf(path, sizeof(path), "%s/fb_%d.ppm", fbdir, gametic_just_run);
        fp = fopen(path, "wb");
        if (fp == NULL)
        {
            OTrace_HarnessError("failed to open framebuffer ppm");
        }
        if (fprintf(fp, "P6\n320 200\n255\n") < 0)
        {
            fclose(fp);
            OTrace_HarnessError("failed writing ppm header");
        }
        for (x = 0; x < 64000; ++x)
        {
            byte idx = I_VideoBuffer[x];
            unsigned char rgb[3];

            rgb[0] = raw_palette[idx * 3 + 0];
            rgb[1] = raw_palette[idx * 3 + 1];
            rgb[2] = raw_palette[idx * 3 + 2];
            if (fwrite(rgb, 1, 3, fp) != 3)
            {
                fclose(fp);
                OTrace_HarnessError("failed writing ppm pixels");
            }
        }
        if (fclose(fp) != 0)
        {
            OTrace_HarnessError("failed closing ppm");
        }

        memcpy(digest_input, I_VideoBuffer, 64000);
        memcpy(digest_input + 64000, raw_palette, 768);
        h = OTrace_FNV1a64(digest_input, 64000 + 768);

        M_snprintf(path, sizeof(path), "%s/fb_%d.fnv", fbdir, gametic_just_run);
        fp = fopen(path, "wb");
        if (fp == NULL)
        {
            OTrace_HarnessError("failed to open framebuffer fnv");
        }
        if (fprintf(fp, "%016llx\n", (unsigned long long)h) < 0)
        {
            fclose(fp);
            OTrace_HarnessError("failed writing fnv file");
        }
        if (fclose(fp) != 0)
        {
            OTrace_HarnessError("failed closing fnv");
        }

        fbtics_dumped[i] = true;
    }
}

static void OTrace_OnFinishUpdate(void)
{
    /* After TryRunTics, gametic has already been incremented past the
       tic that was just simulated and drawn. */
    if (!otrace_active || gametic <= 0)
    {
        return;
    }

    OTrace_DumpFramebuffer(gametic - 1);
}

static unsigned int OTrace_MapThinkerFunc(thinker_t *th)
{
    actionf_v acv = th->function.acv;
    actionf_p1 acp1 = th->function.acp1;

    if (acv == (actionf_v)(-1))
    {
        return THF_REMOVED;
    }
    if (acp1 == NULL || acv == (actionf_v)NULL)
    {
        return THF_NULL;
    }
    if (acp1 == (actionf_p1)P_MobjThinker)
    {
        return THF_MOBJ;
    }
    if (acp1 == (actionf_p1)T_VerticalDoor)
    {
        return THF_VERTICALDOOR;
    }
    if (acp1 == (actionf_p1)T_MoveCeiling)
    {
        return THF_MOVECEILING;
    }
    if (acp1 == (actionf_p1)T_MoveFloor)
    {
        return THF_MOVEFLOOR;
    }
    if (acp1 == (actionf_p1)T_PlatRaise)
    {
        return THF_PLATRAISE;
    }
    if (acp1 == (actionf_p1)T_LightFlash)
    {
        return THF_LIGHTFLASH;
    }
    if (acp1 == (actionf_p1)T_StrobeFlash)
    {
        return THF_STROBEFLASH;
    }
    if (acp1 == (actionf_p1)T_Glow)
    {
        return THF_GLOW;
    }
    if (acp1 == (actionf_p1)T_FireFlicker)
    {
        return THF_FIREFLICKER;
    }

    fprintf(stderr, "otrace: unknown thinker function %p\n", (void *)acp1);
    OTrace_Shutdown();
    exit(2);
}

static void OTrace_EmitPlayer(int i)
{
    player_t *p = &players[i];
    mobj_t *mo = p->mo;
    int a;
    int w;

    OTrace_PutU32((uint32_t)i);

    if (mo == NULL)
    {
        OTrace_PutU32(OTRACE_NO_MO);
        OTrace_PutI32(0);
        OTrace_PutI32(0);
        OTrace_PutI32(0);
        OTrace_PutI32(0);
        OTrace_PutI32(0);
        OTrace_PutI32(0);
        OTrace_PutU32(0);
    }
    else
    {
        OTrace_PutU32(mo->thinker.trace_id);
        OTrace_PutI32(mo->x);
        OTrace_PutI32(mo->y);
        OTrace_PutI32(mo->z);
        OTrace_PutI32(mo->momx);
        OTrace_PutI32(mo->momy);
        OTrace_PutI32(mo->momz);
        OTrace_PutU32(mo->angle);
    }

    OTrace_PutI32(p->viewz);
    OTrace_PutI32(p->health);
    OTrace_PutI32(p->armorpoints);
    OTrace_PutI32((int32_t)p->readyweapon);
    OTrace_PutI32((int32_t)p->pendingweapon);

    for (a = 0; a < NUMAMMO; ++a)
    {
        OTrace_PutI32(p->ammo[a]);
    }
    for (w = 0; w < NUMWEAPONS; ++w)
    {
        OTrace_PutI32(p->weaponowned[w]);
    }

    OTrace_PutI32((int32_t)p->cmd.forwardmove); /* sign-extended from signed char */
    OTrace_PutI32((int32_t)p->cmd.sidemove);
    OTrace_PutI32((int32_t)p->cmd.angleturn); /* sign-extended from short */
    OTrace_PutU32((uint32_t)(unsigned char)p->cmd.buttons);
}

static void OTrace_EmitThinker(thinker_t *th)
{
    unsigned int func = OTrace_MapThinkerFunc(th);

    OTrace_PutU32(th->trace_id);
    OTrace_PutU32(func);

    if (func == THF_MOBJ)
    {
        mobj_t *mobj = (mobj_t *)th;
        int state_index;

        if (mobj->state == NULL)
        {
            OTrace_HarnessError("mobj with NULL state");
        }
        state_index = (int)(mobj->state - states);

        OTrace_PutI32(mobj->x);
        OTrace_PutI32(mobj->y);
        OTrace_PutI32(mobj->z);
        OTrace_PutI32(mobj->momx);
        OTrace_PutI32(mobj->momy);
        OTrace_PutI32(mobj->momz);
        OTrace_PutU32(mobj->angle);
        OTrace_PutI32(state_index);
        OTrace_PutI32(mobj->tics);
        OTrace_PutI32(mobj->health);
        OTrace_PutU32((uint32_t)mobj->flags);
        OTrace_PutI32((int32_t)mobj->type);
    }
}

static uint64_t OTrace_SectorsDigest(void)
{
    unsigned char *buf;
    size_t nbytes;
    int s;
    uint64_t h;

    if (numsectors <= 0)
    {
        return OTrace_FNV1a64(NULL, 0);
    }

    nbytes = (size_t)numsectors * 8u;
    buf = malloc(nbytes);
    if (buf == NULL)
    {
        OTrace_HarnessError("out of memory for sectors digest");
    }

    for (s = 0; s < numsectors; ++s)
    {
        int32_t floor = sectors[s].floorheight;
        int32_t ceil = sectors[s].ceilingheight;
        size_t off = (size_t)s * 8u;

        buf[off + 0] = (unsigned char)((uint32_t)floor & 0xffu);
        buf[off + 1] = (unsigned char)(((uint32_t)floor >> 8) & 0xffu);
        buf[off + 2] = (unsigned char)(((uint32_t)floor >> 16) & 0xffu);
        buf[off + 3] = (unsigned char)(((uint32_t)floor >> 24) & 0xffu);
        buf[off + 4] = (unsigned char)((uint32_t)ceil & 0xffu);
        buf[off + 5] = (unsigned char)(((uint32_t)ceil >> 8) & 0xffu);
        buf[off + 6] = (unsigned char)(((uint32_t)ceil >> 16) & 0xffu);
        buf[off + 7] = (unsigned char)(((uint32_t)ceil >> 24) & 0xffu);
    }

    h = OTrace_FNV1a64(buf, nbytes);
    free(buf);
    return h;
}

void OTrace_EarlyInit(void)
{
    if (M_CheckParmWithArgs("-tracedemo", 1) <= 0)
    {
        return;
    }

    otrace_wanted = true;

    /* Before any SDL video/audio init. */
    if (setenv("SDL_VIDEODRIVER", "dummy", 1) != 0
        || setenv("SDL_AUDIODRIVER", "dummy", 1) != 0)
    {
        OTrace_HarnessError("setenv failed for SDL dummy drivers");
    }
}

void OTrace_DisableSound(void)
{
    if (!otrace_wanted)
    {
        return;
    }

    snd_sfxdevice = SNDDEVICE_NONE;
    snd_musicdevice = SNDDEVICE_NONE;
}

boolean OTrace_Enabled(void)
{
    return otrace_active;
}

boolean OTrace_Init(void)
{
    int p;
    const char *trace_path = NULL;
    const char *digest_path = NULL;

    if (!otrace_wanted)
    {
        return false;
    }

    p = M_CheckParmWithArgs("-trace", 1);
    if (p <= 0)
    {
        OTrace_HarnessError("-tracedemo requires -trace <out.trc>");
    }
    trace_path = myargv[p + 1];

    p = M_CheckParmWithArgs("-digest", 1);
    if (p <= 0)
    {
        OTrace_HarnessError("-tracedemo requires -digest <out.dig>");
    }
    digest_path = myargv[p + 1];

    maxtics = 0;
    p = M_CheckParmWithArgs("-maxtics", 1);
    if (p > 0)
    {
        maxtics = atoi(myargv[p + 1]);
        if (maxtics <= 0)
        {
            OTrace_HarnessError("-maxtics must be a positive integer");
        }
    }

    fbtics_count = 0;
    fbdir = NULL;
    p = M_CheckParmWithArgs("-fbtics", 1);
    if (p > 0)
    {
        OTrace_ParseFbtics(myargv[p + 1]);
    }
    p = M_CheckParmWithArgs("-fbdir", 1);
    if (p > 0)
    {
        fbdir = myargv[p + 1];
        M_MakeDirectory(fbdir);
    }
    if (fbtics_count > 0 && fbdir == NULL)
    {
        OTrace_HarnessError("-fbtics requires -fbdir");
    }

    /* Config must not override dummy video. */
    video_driver = "";
    if (setenv("SDL_VIDEODRIVER", "dummy", 1) != 0
        || setenv("SDL_AUDIODRIVER", "dummy", 1) != 0)
    {
        OTrace_HarnessError("setenv failed for SDL dummy drivers");
    }

    trc_fp = fopen(trace_path, "wb");
    if (trc_fp == NULL)
    {
        OTrace_HarnessError("failed to open -trace output");
    }
    dig_fp = fopen(digest_path, "wb");
    if (dig_fp == NULL)
    {
        OTrace_HarnessError("failed to open -digest output");
    }

    OTrace_WriteHeader(trc_fp, "DTRC");
    OTrace_WriteHeader(dig_fp, "DDIG");

    I_OnSetPalette = OTrace_OnSetPalette;
    I_OnFinishUpdate = OTrace_OnFinishUpdate;

    I_AtExit(OTrace_Shutdown, true);

    otrace_active = true;
    records_emitted = 0;
    return true;
}

void OTrace_Shutdown(void)
{
    if (otrace_shutting_down)
    {
        return;
    }
    otrace_shutting_down = true;

    I_OnSetPalette = NULL;
    I_OnFinishUpdate = NULL;

    if (trc_fp != NULL)
    {
        fflush(trc_fp);
        fclose(trc_fp);
        trc_fp = NULL;
    }
    if (dig_fp != NULL)
    {
        fflush(dig_fp);
        fclose(dig_fp);
        dig_fp = NULL;
    }

    free(rec_buf);
    rec_buf = NULL;
    rec_cap = 0;
    rec_len = 0;

    otrace_active = false;
}

void OTrace_VerifyDummyVideo(void)
{
    const char *driver;

    if (!otrace_wanted)
    {
        return;
    }

    driver = SDL_GetCurrentVideoDriver();
    if (driver == NULL || strcmp(driver, "dummy") != 0)
    {
        I_Error("otrace: SDL video driver is '%s', expected 'dummy'",
                driver ? driver : "(null)");
    }
}

void OTrace_TicDump(void)
{
    uint32_t in_level;
    uint32_t player_count;
    uint32_t thinker_count;
    uint32_t active_sector_count;
    uint64_t sectors_digest;
    uint64_t tic_digest;
    unsigned char dig_bytes[8];
    int i;
    thinker_t *th;

    if (!otrace_active)
    {
        return;
    }

    OTrace_RecReset();

    in_level = (gamestate == GS_LEVEL) ? 1u : 0u;

    OTrace_PutU32((uint32_t)gametic);
    OTrace_PutU32(in_level);

    if (!in_level)
    {
        OTrace_PutU32(0); /* leveltime */
        OTrace_PutU32((uint32_t)rndindex);
        OTrace_PutU32((uint32_t)prndindex);
        OTrace_PutU32(0); /* player_count */
        OTrace_PutU32(0); /* thinker_count */
        OTrace_PutU32(0); /* active_sector_count */
        OTrace_PutU64(OTRACE_FNV_OFFSET); /* empty-string FNV */
    }
    else
    {
        /* Count players */
        player_count = 0;
        for (i = 0; i < MAXPLAYERS; ++i)
        {
            if (playeringame[i])
            {
                player_count++;
            }
        }

        /* Count thinkers */
        thinker_count = 0;
        for (th = thinkercap.next; th != &thinkercap; th = th->next)
        {
            thinker_count++;
        }

        /* Count active sectors */
        active_sector_count = 0;
        for (i = 0; i < numsectors; ++i)
        {
            if (sectors[i].specialdata != NULL)
            {
                active_sector_count++;
            }
        }

        sectors_digest = OTrace_SectorsDigest();

        OTrace_PutU32((uint32_t)leveltime);
        OTrace_PutU32((uint32_t)rndindex);
        OTrace_PutU32((uint32_t)prndindex);

        OTrace_PutU32(player_count);
        for (i = 0; i < MAXPLAYERS; ++i)
        {
            if (playeringame[i])
            {
                OTrace_EmitPlayer(i);
            }
        }

        OTrace_PutU32(thinker_count);
        for (th = thinkercap.next; th != &thinkercap; th = th->next)
        {
            OTrace_EmitThinker(th);
        }

        OTrace_PutU32(active_sector_count);
        for (i = 0; i < numsectors; ++i)
        {
            if (sectors[i].specialdata != NULL)
            {
                OTrace_PutU32((uint32_t)i);
                OTrace_PutI32(sectors[i].floorheight);
                OTrace_PutI32(sectors[i].ceilingheight);
            }
        }

        OTrace_PutU64(sectors_digest);
    }

    if (fwrite(rec_buf, 1, rec_len, trc_fp) != rec_len)
    {
        OTrace_HarnessError("failed writing tic record");
    }

    tic_digest = OTrace_FNV1a64(rec_buf, rec_len);
    dig_bytes[0] = (unsigned char)(tic_digest & 0xffu);
    dig_bytes[1] = (unsigned char)((tic_digest >> 8) & 0xffu);
    dig_bytes[2] = (unsigned char)((tic_digest >> 16) & 0xffu);
    dig_bytes[3] = (unsigned char)((tic_digest >> 24) & 0xffu);
    dig_bytes[4] = (unsigned char)((tic_digest >> 32) & 0xffu);
    dig_bytes[5] = (unsigned char)((tic_digest >> 40) & 0xffu);
    dig_bytes[6] = (unsigned char)((tic_digest >> 48) & 0xffu);
    dig_bytes[7] = (unsigned char)((tic_digest >> 56) & 0xffu);
    if (fwrite(dig_bytes, 1, 8, dig_fp) != 8)
    {
        OTrace_HarnessError("failed writing digest entry");
    }

    records_emitted++;

    if (maxtics > 0 && records_emitted >= maxtics)
    {
        OTrace_Shutdown();
        exit(0);
    }
}
