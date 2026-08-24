#!/usr/bin/env bash
# Configure and build the Chocolate Doom oracle binary into oracle/build/.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
SRC="${ROOT}/chocolate-doom"
BUILD="${ROOT}/build"

# Reproducible archive member timestamps (Apple ld / ar).
export ZERO_AR_DATE=1

# -DNDEBUG kept for Release-equivalent semantics (assert elision). Constraint is
# optimization level: last -O must be -O2. Empty CMAKE_C_FLAGS_RELEASE stops
# CMake's Release defaults from appending -O3 after CMAKE_C_FLAGS.
CFLAGS_EXACT="-O2 -fno-strict-aliasing -DNDEBUG"
# LC_UUID is kept: modern dyld refuses to load binaries without it, and Apple ld
# derives the UUID deterministically from a hash of the binary content.
LDFLAGS_EXACT=""

rm -rf "${BUILD}"
mkdir -p "${BUILD}"

cmake -S "${SRC}" -B "${BUILD}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_FLAGS="${CFLAGS_EXACT}" \
  -DCMAKE_C_FLAGS_RELEASE="" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_EXE_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DCMAKE_SHARED_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DCMAKE_MODULE_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DENABLE_SDL2_MIXER=ON \
  -DENABLE_SDL2_NET=ON

cmake --build "${BUILD}" --target chocolate-doom -j"$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "Built: ${BUILD}/src/chocolate-doom"
shasum -a 256 "${BUILD}/src/chocolate-doom"
