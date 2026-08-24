#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
exec lake exe perf-encode
