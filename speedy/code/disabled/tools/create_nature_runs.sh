#!/usr/bin/env bash
#=======================================================================
#> @brief Runs create_nature_run for different time_steps using 
#>        appropiate nature names
#>
#> Usage example:
#=======================================================================

create_nature_runs(){
    [[ $# -eq 2 ]] || error "Usage: create_nature_runs spinup_name steps"
        spinup_name=$1
              steps=$2
#    time_step_array=$3

    echo "==========================================================="
    echo " GENERATING A SET OF NATURE RUNS WITH DIFFERENT TIME STEPS"
    echo "==========================================================="

    cd $ARCHIVE_DIR
    rm -f touch nature_runs.dat; touch nature_runs.dat
    for time_step in ${time_step_array[@]}
    do
        # Nature name generation
            s_steps=$(printf '%04d' $steps)
        s_time_step=$(printf '%04d' $time_step)
        case "$spinup_name" in
             "spinup_1979010100-start_001-yr") s_spinup="spinup1";;
                                            *) error "Unknown spinup_name $spinup_name";;
        esac            
        nature_name="nature_${s_time_step}hr_${s_steps}ts_${s_spinup}"
        echo $nature_name >> nature_runs.dat
        echo " Creating $nature_name"
        time -p nature_run.sh $spinup_name $time_step $steps $nature_name | tee ${nature_name}.log &

    done

    wait

    # Store logs
    cat nature_runs.dat | while read nature_name; do 
        mv ${nature_name}.log ${nature_name}/${nature_name}.log
    done
    
    rm -f nature_runs.dat
    echo "==========================================================="
    echo " NATURE RUNS GENERATED SUCCESSFULLY"
    echo "==========================================================="

    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_nature_runs $@
exit $?
