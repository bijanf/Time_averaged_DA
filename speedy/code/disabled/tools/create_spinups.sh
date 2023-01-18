#!/usr/bin/env bash
#=======================================================================
#> @brief Runs spinup_model for different time_lengths using 
#>        appropiate spinup names
#=======================================================================
create_spinups(){
    [[ $# -eq 1 ]] || error "Usage: create_spinups time_start"
    time_start=$1
       
    echo "==============================================="
    echo " GENERATING A SPINUP SET "
    echo "==============================================="
    
    cd $ARCHIVE_DIR
    rm -f spinups.dat; touch spinups.dat
    for time_length in ${time_length_array[@]}
    do
        time_length_hr=$(( 8640 * time_length ))

        echo " Generating name"
        s_time_length=$(printf '%03d' $time_length)
        spinup_name="spinup_${time_start}-start_${s_time_length}-yr"
        echo $spinup_name >> spinups.dat

        echo " Creating $spinup_name"
		time spinup.sh $time_start $time_length_hr $spinup_name $ARCHIVE_DIR | tee ${spinup_name}.log &
		#time -p spinup.sh $time_start $time_length_hr $spinup_name $ARCHIVE_DIR > ${spinup_name}.log &
    done

wait
    echo "Storing logs"
    cat spinups.dat | while read spinup_name; do 
        mv ${spinup_name}.log ${spinup_name}/${spinup_name}.log
    done
    
    rm -f spinups.dat
    echo "==============================================="
    echo " spinup RUNS GENERATED SUCCESSFULLY"
    echo "==============================================="

    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_spinups $@
exit $?
