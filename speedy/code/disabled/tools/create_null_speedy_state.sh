#!/usr/bin/env bash
#=======================================================================
#> @brief Creates an Speedy restart file with all variables set to zero
#> and its corresponding control file
#=======================================================================
create_null_speedy_state()
{
# 	[[ $# -eq 1 ]] || error "Usage: calculate_variance binary_file_list"
#	binary_file_list=$1
#	THERE=$(pwd); cd $THERE
	echo "==============================================="
	echo " Null speedy state generation"
	echo "==============================================="
	
	HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
	cd $HERE

	build_program.sh create_null_speedy_state

	echo " - Creating binary file"
	./create_null_speedy_state.exe

	echo " - Creating descriptor file"
	(
cat<<_EOF_
DSET ^null_speedy_state_grid_sigma.rst
TITLE Null SPEEDY Model Prognostic Variables
UNDEF -9.99E33
OPTIONS template big_endian
XDEF 96 LINEAR 0.0 3.75
YDEF 48 LEVELS  -87.159 -83.479 -79.777 -76.070 -72.362 -68.652 -64.942 -61.232 -57.521 -53.810 -50.099 -46.389 -42.678 -38.967 -35.256 -31.545 -27.833 -24.122 -20.411 -16.700 -12.989  -9.278  -5.567  -1.856   1.856   5.567   9.278  12.989  16.700  20.411  24.122  27.833  31.545  35.256  38.967  42.678  46.389  50.099  53.810  57.521  61.232  64.942  68.652  72.362  76.070  79.777  83.479  87.159
ZDEF 7 LEVELS  0.950 0.835 0.685 0.510 0.340 0.200 0.080
TDEF 1 LINEAR 06Z01FEB1979 6HR
VARS 6
U 7 99 U-wind [m/s]
V 7 99 V-wind [m/s]
T 7 99 Temperature [K]
Q 7 99 Specific Humidity [kg/kg]
PS 0 99 Surface Pressure [Pa]
RAIN 0 99 Precipitation [mm/6hr]
ENDVARS
_EOF_
	)>"null_speedy_state_grid_sigma.ctl"

	rm -f create_null_speedy_state.exe
	echo "==============================================="
	echo " Binary and control files were created"
	echo "==============================================="
	
	return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_null_speedy_state $@
exit $?


