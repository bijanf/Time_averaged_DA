#!/bin/bash
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

#-----------------------------------------
# - Sourcing DA system function libraries
#-----------------------------------------
. scripts/system_functions.sh
. scripts/compiling_functions.sh
. scripts/time_functions.sh
. scripts/ta_low_level_functions.sh
. scripts/ta_high_level_functions.sh
. scripts/ta_plotting_functions.sh

#. scripts/da_low_level_functions_1.sh
#. scripts/da_low_level_functions_2.sh
#. scripts/da_plotting_functions.sh
#. scripts/da_high_level_functions.sh
