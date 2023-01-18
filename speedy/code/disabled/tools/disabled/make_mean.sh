#!/usr/bin/env bash
#=======================================================================
#> @brief Compiles calculate_variance.f90
#=======================================================================

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $HERE

build_program.sh mean
