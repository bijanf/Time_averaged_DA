#!/usr/bin/env bash
#=========================================================================
#> @brief Performs a data assimilation experiment with SPEEDY model
#=========================================================================
da_experiment()
{
    [ $# -eq 4 ] || error "Usage: da_experiment nature_name members sampling_name processors"
      nature_name=$1
          members=$2
    sampling_name=$3
       processors=$4
    
    echo "==============================================="
    echo " Performing DA experiment"
    echo "==============================================="
       
    exp_name="DAexp_m${members}_${nature_name}"
     storing_dir=$ARCHIVE_DIR
      nature_dir=${storing_dir}/${nature_name}
    sampling_dir=${storing_dir}/${sampling_name}
         exp_dir=${storing_dir}/${exp_name}
#         log_dir=${exp_name}/log

    create_archive_dir.sh $exp_dir || return 0
    cd "$exp_dir"; mkdir log
    
    echo " - Observing nature run $nature_name"
    create_observations.sh $nature_name "max_tree_network" obs "$exp_dir" > log/create_observations_sh.log

    echo " - Performing a constrained ensemble run"
    ensemble_run.sh $exp_dir "assim" $members $sampling_dir $nature_dir $processors #> log/assim_run.log
    
    echo " - Performing a free ensemble run"
    ensemble_run.sh $exp_dir "free" $members $sampling_dir $nature_dir $processors > log/free_run.log

    echo "==============================================="
    echo " DA experiment ended successfully"
    echo "==============================================="
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

da_experiment "nature_6hr_100steps" 20 "sampling_100steps_12hours" 4

#da_experiment $@
exit $?
