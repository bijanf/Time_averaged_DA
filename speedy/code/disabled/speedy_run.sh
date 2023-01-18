#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#=======================================================================
#> @brief Wrapper script to run SPEEDY executable at_gcm.exe
#> at_gcm.exe, restart and forcing files must be in the same folder.
#> Restart and forcing files can be also just soft links.
#> @note See @ref da_files for details
# ! da_files is at the bottom of this file
#=======================================================================
speedy_run(){
    [ $# -eq 5 ] || error "Usage: speedy_run istart output_flag time_start time_leap output_dir"

    istart=$1 # Input  restart mode (info in com_tsteps.h)
    output_flag=$2 # Output restart mode (info in com_tsteps.h)
    time_start=$3
    time_leap=$4
    output_dir="$5"

    time_forecast=$(time_increment.exe $time_start $time_leap)
    echo "$time_forecast" > time_forecast.dat

    # Setting restart mode and initial time
    echo $istart                      > fort.2
    echo $output_flag                >> fort.2

    # Set initial time
    echo ${time_start:0:4}  > fort.7 # year
    echo ${time_start:4:2} >> fort.7 # month
    echo ${time_start:6:2} >> fort.7 # day
    echo ${time_start:8:2} >> fort.7 # hour

    # Set final time
    echo ${time_forecast:0:4}  > fort.8 # year
    echo ${time_forecast:4:2} >> fort.8 # month
    echo ${time_forecast:6:2} >> fort.8 # day
    echo ${time_forecast:8:2} >> fort.8 # hour

    # Time leap info needed for ctl file creation
    echo $time_leap > fort.9

    # Set initial conditions (restart files)
    case "$istart" in
        0|1|2) # Reference atmosphere|spectral Instantaneous|gridded Instantaneous
            ;;
        3)     # Time average decomposed
            ln -fs ${time_start}_grid_sigma_Taver.rst fort.92
            ln -fs ${time_start}_grid_sigma_Tanom.rst fort.93
            ;;
        *) error "Invalid istart value $istart";;
    esac

    # Running speedy
    at_gcm.exe # > speedy.log
    # ./at_gcm.exe # > speedy.log

    # Storing final state
    [[ "$output_dir" != "." ]] && mv ${time_forecast}* $output_dir

    # Renaming spectral restart file
    #[ $output_flag == 1 ] && mv fort.10 ${time_forecast}_spect_sigma.rst
    # if [ ! "$(pwd)" == "$output_dir" ]; then
##          if [ "$output_flag" == 2 ]; then
  ## Reference atmosphere|spectral Instantaneous|gridded Instantaneous
#mv ${time_forecast}_grid_sigma_Insta.rst ${output_dir}
 #mv ${time_forecast}_grid_press_Insta.rst ${output_dir}
##              mv ${time_forecast}_grid_sigma_Taver.rst ${output_dir}
##               mv ${time_forecast}_grid_sigma_Insta.ctl ${output_dir}
##          fi

#if [ "$output_flag" == 3 ] || [ "$output_flag" == 4 ]; then
  ## equal to 2 plus Gridded Sigma Time-averaged state| and Gridded Sigma Time-averaged anomaly
##              mv ${time_forecast}_grid_sigma_Insta.rst ${output_dir}
##                  mv ${time_forecast}_grid_press_Insta.rst ${output_dir}
 #mv ${time_forecast}_grid_sigma_Taver.rst ${output_dir}
 ## Anomaly is not moved to comply with the analysis cycle design
 ## Both input restarts must be in postr folder
 #fi
 ## ;;
##              *) error "Invalid output_flag value $output_flag";;
##          esac
#           mv ${time_forecast}*.ctl ${output_dir}
# mv ${time_forecast}* ${output_dir}

 # fi
 # Storing Spectraly filtered analysis fields (currently disabled)
 #mv ${time_start}_TA.grd ${archive_dir}/anal_f/${member}
 #mv ${time_start}_AN.grd ${archive_dir}/anal_f/${member}

    return 0 # Success
}

speedy_run $@
exit $?