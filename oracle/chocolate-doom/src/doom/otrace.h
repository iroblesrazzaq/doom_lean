// Trace harness writer for Doom Lean verification (TRACE.md v1).
// Tracing only — does not alter playsim behavior when inactive.

#ifndef __OTRACE_H__
#define __OTRACE_H__

#include "doomtype.h"

// Early setup when -tracedemo is present: dummy SDL drivers, etc.
void OTrace_EarlyInit(void);

// Parse -trace/-digest/-maxtics/-fbtics/-fbdir; open files; enable dumping.
// Returns true if tracing is active. Aborts with exit(2) on harness errors.
boolean OTrace_Init(void);

// True when -tracedemo tracing is enabled.
boolean OTrace_Enabled(void);

// Dump one tic record immediately after G_Ticker (no-op if inactive).
void OTrace_TicDump(void);

// Flush and close trace files (safe to call multiple times).
void OTrace_Shutdown(void);

// After I_InitGraphics: require SDL video driver "dummy".
void OTrace_VerifyDummyVideo(void);

// Force no sound/music devices (call before I_InitSound when tracing).
void OTrace_DisableSound(void);

#endif
