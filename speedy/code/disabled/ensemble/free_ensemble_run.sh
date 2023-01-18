#!/usr/bin/env bash
#===========================================================================
#> @brief Performs an ensemble run without data assimilation which will be
#> as reference to study the skill of data assimilation ensemble runs.
#free_ensemble_run free-run_0006hr_0002ts_m20_samp2 free-run_0006hr_0002ts_m20_samp2 station_homogeneous_gap2 original Insta  | tee $ARCHIVE_DIR/free-run_0006hr_0002ts_m20_samp2.log
#=========================================================================

free_ensemble_run(){
    [[ $# -eq 5 ]] || error "Usage: free_ensemble_run run_name nature_name sampling_name members save_members"
    free_run_name=$1
      nature_name=$2
    sampling_name=$3
          members=$4
     save_members=$5
    echo "==============================================="
    echo " FREE ENSEMBLE RUN"
    echo "==============================================="

    export SNR="1.0"
    ensemble_run.sh $free_run_name $nature_name "station_none" $sampling_name "STemp" "Insta" $members $save_members
    #                  run_name  nature_name station_set_name sampling_name obs_config A_mode members  save_members

    echo "free-run" > $ARCHIVE_DIR/$free_run_name/dataset_type.dat
    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

free_ensemble_run $@
exit $?
