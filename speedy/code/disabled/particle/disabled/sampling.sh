#!/usr/bin/env bash
#=========================================================================
#> @brief Performs a model run which will be used as the "true" evolution.
#> Output is saved in $ARCHIVE_DIR.
#=========================================================================
sampling(){
#    funct_opening always
    [[ $# -eq 4 ]] || error "Usage: sampling spinup_name time_step steps sampling_name"

      spinup_name=$1
        time_step=$2
            steps=$3
    sampling_name=$4

    echo "==============================================="
    echo " SAMPLING GENERATION"
    echo "==============================================="

    trajectory.sh $spinup_name $time_step $steps $sampling_name 2 $ARCHIVE_DIR
    echo "sampling" > $ARCHIVE_DIR/$spinup_name/dataset_type.dat

    #funct_closing always
   	return 0 # Success
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

sampling $@
exit $?
