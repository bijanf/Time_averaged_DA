#!/usr/bin/env bash
#=========================================================================
#> @brief Performs a model run which will be used as the "true" evolution.
#> Instanteneous and time-average states are saved in $ARCHIVE_DIR.
#>
#> Usage example:
#> $ nature_run.sh spinup_60hr_1980010100 6 10 nature_6hr_10st
#=========================================================================

nature_run(){
    [[ $# -eq 4 ]] || error "Usage: nature_run spinup_name time_step steps nature_run_name"
          spinup_name=$1
            time_step=$2
                steps=$3
      nature_run_name=$4

    echo "==============================================="
    echo " NATURE RUN"
    echo "==============================================="

    trajectory.sh $spinup_name $time_step $steps $nature_run_name 3 $ARCHIVE_DIR
    echo "nature-run" > $ARCHIVE_DIR/$nature_run_name/dataset_type.dat

    return 0 # Success
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

# Previous hardwirings
#nature_run spinup_1979_1year 6 10 nature_6hr_10steps | tee $ARCHIVE_DIR/nature_6hr_10steps.log
#nature_run spinup_1979_1year 6 100 nature_6hr_100steps | tee $ARCHIVE_DIR/nature_6hr_100steps.log
#nature_run spinup_1979_1year 6 100 nature_6hr_100steps | tee $ARCHIVE_DIR/nature_6hr_100steps.log

nature_run $@
exit $?
