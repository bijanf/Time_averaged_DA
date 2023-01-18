#!/usr/bin/env bash
#=======================================================================
#> @brief Runs create_nature_run for different time_steps using 
#>        appropiate nature names
#>
#> Usage example:
#=======================================================================
create_assim_ensemble_runs(){
    [[ $# -eq 5 ]] || error "Usage: create_assim_ensemble_runs station_set_name obs_config A_mode pattern"
    station_set_name=$1
          obs_config=$2
                 SNR=$3
              A_mode=$4
             pattern=$5

    echo "==========================================================="
    echo " GENERATING A SET OF ASSIM RUNS GIVEN A LIST OF FREE RUNS"
    echo "==========================================================="

    cd $ARCHIVE_DIR
    free_run_list="free_runs.dat"
    find . -maxdepth 1 -type d -name "${pattern}" | sort > $free_run_list

    cat $free_run_list | while read free_run_name; do 
        echo " - Free run: $free_run_name"

        case "$station_set_name" in
        "station_homogeneous_gap2" ) s_station="Homo2";;
        "station_tree_coverage_max") s_station="TreeMax";;
                                  *) error "Unknown station_set_name $station_set_name";;
        esac        
        echo " - Station id (s_station) = $s_station"
        
        case $obs_config in
            "Origi" ) # Default Speedy-Letkf configuration
                assim_run_name="assim-run_${free_run_name:11:23}_sta-${s_station}_obs-${obs_config}_${A_mode}-DA"
                ;;
            "STemp") # Only surface temperature measurements, error proportional to variance thus SNR is needed
                assim_run_name="assim-run_${free_run_name:11:23}_sta-${s_station}_obs-${obs_config}_SNR-${SNR}_${A_mode}-DA"
                export SNR
                ;;
             *) error "Unknown obs_config $obs_config";;
        esac            
        echo " - assim_run_name = $assim_run_name"

        echo " - Launching create_assim_ensemble_run.sh"
        time -p assim_ensemble_run.sh $assim_run_name $free_run_name $station_set_name $obs_config $A_mode | tee ${assim_run_name}.log
        mv ${assim_run_name}.log ${assim_run_name}/${assim_run_name}.log

    done

    rm -f $free_run_list
    
    echo "==========================================================="
    echo " ASSIMILATION RUNS GENERATED SUCCESSFULLY"
    echo "==========================================================="

    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_assim_ensemble_runs $@
exit $?
