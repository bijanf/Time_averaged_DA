#!/usr/bin/env bash
#set -x
#=========================================================================
#> @brief Runs SPEEDY model and stores restart, control and metainfo
#> files as a trajectory dataset
#=========================================================================
trajectory(){
    [ $# -eq 6 ] || error "Usage: trajectory spinup_name time_step steps trajectory_name output_flag storing_dir"
    spinup_dir=$1
    time_step=$2 # Multiple of 6 hours
    steps=$3
    trajectory_dir=$4
    output_flag=$5
    storing_dir=$6

    echo "==============================================="
    echo " Creating trajectory"
    echo "==============================================="

    # trajectory_dir=${storing_dir}/${trajectory_name}
        # spinup_dir=${storing_dir}/${spinup_name}
    spinup_name=$(basename $spinup_dir)

    echo " - Parsing Arguments"

    # No starting state?
    [ -d $spinup_dir ] || error "There is no spinup dataset '$spinup_name' in $storing_dir"

    # too big model output?
    steps_limit=300
    if [ $steps -gt $steps_limit ]; then
        space_needed=$(echo "$steps_limit * 0.540" | bc)
        echo "Approx $space_needed Mbytes of model output will be store in the archive!!"
        read -p "Do you want to continue? [Yy]* / * " yn
        case $yn in
            [Yy]* ) ;;
            * ) return;;
        esac
    fi

    echo " - Setting enviroment"
    create_archive_dir.sh $trajectory_dir || return 0
    cd $trajectory_dir

    mkdir states; cd states
#    cp ${spinup_dir}/*.ctl .        # Copying initial state descriptor
    cp $SPEEDY/model/tmp/at_gcm.exe .     # Copying SPEEDY executable
    speedy_link_forcings.sh "t30" .

    echo " - Setting Time interval" # (Starts from the end of spinup)"
    time_zero=$(cat ${spinup_dir}/time_final.dat)
    cp ${spinup_dir}/${time_zero}_grid_sigma_Insta.rst . # Copying initial state

    time_start=$(time_increment.exe $time_zero $time_step)
    # time_length=$(( time_step * (steps - 1) ))
    time_length=$(echo "$time_step * ($steps - 1)"|bc)
    time_final=$(time_increment.exe $time_start $time_length)

    echo "  -----------------------------------------------"
    echo "   time_start  = $time_start"
    echo "   time_length = $time_length hours"
    echo "   time_final  = $time_final"
    echo "  -----------------------------------------------"

    # Initilize stuff
    touch trajectory.log
    [ $output_flag -ge 2 ] && touch grid_sigma_Insta_states.dat
    [ $output_flag -ge 2 ] && touch grid_press_Insta_states.dat
    [ $output_flag -ge 3 ] && touch grid_sigma_Taver_states.dat

    echo " - Cycling over time"
    present_time=$time_zero; echo " t = $present_time"

    while [ $present_time -lt $time_final ]
    do
        # Run SPEEDY
        #time_forecast=$(time_increment.exe $present_time $time_step)
        here=$(pwd)
        speedy_run.sh 2 $output_flag $present_time $time_step $here >> trajectory.log
        time_forecast=$(cat time_forecast.dat)

            # Store metainfo
        [ $output_flag -ge 2 ] && cat grid_sigma_Insta_state_name.dat >> grid_sigma_Insta_states.dat
        [ $output_flag -ge 2 ] && cat grid_press_Insta_state_name.dat >> grid_press_Insta_states.dat
        [ $output_flag -ge 3 ] && cat grid_sigma_Taver_state_name.dat >> grid_sigma_Taver_states.dat

        # Advance time
        present_time=$time_forecast; echo " t = $present_time"
    done

    echo " - Creating descriptor files"
    [ $output_flag -ge 2 ] && mv ${time_start}_grid_sigma_Insta.ctl YYYYMMDDHH_grid_sigma_Insta.ctl
    [ $output_flag -ge 2 ] && mv ${time_start}_grid_press_Insta.ctl YYYYMMDDHH_grid_press_Insta.ctl
    [ $output_flag -ge 3 ] && mv ${time_start}_grid_sigma_Taver.ctl YYYYMMDDHH_grid_sigma_Taver.ctl

    echo " - Cleaning up"
    [ $output_flag -ge 2 ] && rm -f grid_sigma_Insta_state_name.dat
    [ $output_flag -ge 2 ] && rm -f grid_press_Insta_state_name.dat
    [ $output_flag -ge 3 ] && rm -f grid_sigma_Taver_state_name.dat
    rm time_forecast.dat
    rm ${time_zero}*.rst
    rm -f [0-9]*.ctl
    rm -f at_gcm.exe
    rm -f fort.*

    echo "-----------------------------------------------"
    echo " - Saving metainformation"
    cd ..
    echo $time_zero   >   time_zero.dat
    echo $time_start  >  time_start.dat
    echo $time_final  >  time_final.dat
    echo $time_step   >   time_step.dat
    echo $steps       >       steps.dat
    echo $spinup_name > spinup_name.dat
    touch dataset_complete.dat

    echo "-----------------------------------------------"
    echo " Trajectory dataset saved in"
    echo " $trajectory_dir"
    echo "==============================================="
    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

trajectory $@
exit $?
