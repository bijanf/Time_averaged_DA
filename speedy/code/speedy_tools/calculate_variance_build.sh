#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
cd "$(dirname "${BASH_SOURCE[0]}")"

build_program.sh calculate_variance
