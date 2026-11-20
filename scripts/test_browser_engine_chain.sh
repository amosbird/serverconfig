#!/usr/bin/env bash
set -euo pipefail

: "${ABUSH_ENGINE_FIXTURE:?Set ABUSH_ENGINE_FIXTURE=1 for a fresh isolated Chrome fixture}"
: "${ABUSH_BROWSER_ENGINE_ROOT:?Set the pinned abush browser-engine directory}"
: "${ABUSH_ENGINE_CHROME:?Set the fresh headless Chromium executable}"
[ "$ABUSH_ENGINE_FIXTURE" = 1 ]
serverconfig=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
abush=$(cd -- "$ABUSH_BROWSER_ENGINE_ROOT/.." && pwd -P)
export ABUSH_BROWSER_HOST_PATH="$serverconfig/chromium/external-links/host.py"
export ABUSH_BROWSER_EXTENSION_PATH="$serverconfig/chromium/external-links/extension"
cd -- "$abush"
exec scripts/test-bounded.sh -p abush-daemon --test browser_engine_chain -- --nocapture
