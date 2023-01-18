#!/usr/bin/env bash
#=======================================================================
#> @brief Runs free_ensemble_run for different already existing nature
#         runs using appropiate free ensemble names
#=======================================================================
create_free_ensemble_runs(){
    [[ $# -eq 4 ]] || error "Usage: create_free_ensemble_runs sampling_name members save_members pattern"
    sampling_name=$1
          members=$2
     save_members=$3
          pattern=$4
          
    echo "==========================================================="
    echo " GENERATING A SET OF FREE RUNS GIVEN A LIST OF FREE RUNS"
    echo "==========================================================="

    cd $ARCHIVE_DIR
    nature_run_list="nature_runs.dat"
    find . -maxdepth 1 -type d -name "${pattern}" | sort | cut -c3- > $nature_run_list
#    ls "${pattern}/" > $nature_run_list

    cat $nature_run_list | while read nature_run_name; do 

        # Setting free_run_name
        case "$sampling_name" in
        "sampling_100steps_12hours") s_sampling="samp2";;
                                  *) error "Unknown sampling_name $sampling_name";;
        esac
                    
        free_run_name="free-run_${nature_run_name:7:13}_m${members}_${s_sampling}"
        #free_run_name="free-run_${nature_run_name}_m${members}_${s_sampling}"

        # Launching create_free_ensemble_run.sh
        echo " - Creating ${free_run_name}"
        time -p free_ensemble_run.sh $free_run_name $nature_run_name $sampling_name $members $save_members | tee ${free_run_name}.log
        mv ${free_run_name}.log ${free_run_name}/${free_run_name}.log

    done

    rm -f $nature_run_list
    
    echo "==========================================================="
    echo " FREE RUNS GENERATED SUCCESSFULLY"
    echo "==========================================================="

    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_free_ensemble_runs $@
exit $?
