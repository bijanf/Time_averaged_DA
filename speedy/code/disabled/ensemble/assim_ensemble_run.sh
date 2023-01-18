#!/usr/bin/env bash
#===========================================================================
#> @brief Performs an ensemble run with data assimilation. Run configuration
#> is mostly taken from the free ensemble run it will be compared against.
#assim_ensemble_run assim-run_0006hr_0002ts_m20_samp2 free-run_0006hr_0002ts_m20_samp2 station_homogeneous_gap2 original Insta  | tee $ARCHIVE_DIR/assim-run_0006hr_0002ts_m20_samp2.log
#=========================================================================

assim_ensemble_run(){
    [[ $# -eq 5 ]] || error "Usage: assim_ensemble_run assim_run_name free_run_name station_set_name obs_config A_mode"
      assim_run_name=$1
       free_run_name=$2
    station_set_name=$3
          obs_config=$4
              A_mode=$5

    echo "==============================================="
    echo " DATA ASSIMILATION ENSEMBLE RUN"
    echo "==============================================="

     storing_dir=$ARCHIVE_DIR
    free_run_dir=${storing_dir}/${free_run_name}

    echo " - Parsing Arguments"

    [ -d   $free_run_dir ] || error "There is no free run dataset '$free_run_name' in $storing_dir"

    echo " - Setting run according to free ensemble run"

    cd ${free_run_dir}
    [ -f dataset_complete.dat ] || error "Free Run dataset is not complete"
         nature_name=$(cat nature_name.dat)
       sampling_name=$(cat sampling_name.dat)
             members=$(cat members.dat)
        save_members=$(cat save_members.dat)

    ensemble_run.sh $assim_run_name $nature_name $station_set_name $sampling_name $obs_config $A_mode $members $save_members
    
    echo "assim-run"      > $ARCHIVE_DIR/$assim_run_name/dataset_type.dat
    echo "$free_run_name" > $ARCHIVE_DIR/$assim_run_name/free_run_name.dat
    
    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

assim_ensemble_run $@
exit $?
