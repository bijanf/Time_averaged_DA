#!/usr/bin/env bash
#=========================================================================
#> @brief Harvests observations from a nature run
#>
#> Usage example:
#> $ create_observations.sh "nature_6hr_100steps" "max_tree_network" "obs_1" "$ARCHIVE_DIR"
#=========================================================================
create_observations(){
    [[ $# -eq 5 ]] || error "Usage: create_observations nature_name station_set_name obs_config A_mode storing_dir"
           nature_dir=$1
     station_set_name=$2
           obs_config=$3
               A_mode=$4
          storing_dir=$5 
          
    echo "==============================================="
    echo " Creating observations"
    echo "==============================================="

    station_set=${STATIONS}/${station_set_name}.tbl
    obs_set_dir=${storing_dir}/obs
    
    echo " Parsing Arguments"
    [ -f $station_set ] || error "There is no station_set table '$station_set_name' in $STATIONS"

    create_archive_dir.sh $obs_set_dir || return 0


    case $obs_config in
        "original" ) 
            obs_program=obsmake.exe
            cp $DAS/obs/obserr.tbl $obs_set_dir
#            ln -fs $DAS/speedy/common/orography_t30.dat $obs_set_dir/fort.21  # Set surface geopotential
            ;;
        "surf_temp") 
            obs_program=create_observations.exe
            cd $nature_dir
            ;;
         *) error "Unknown obs_config $obs_config";;
    esac            

    case $A_mode in
        "Free"|"Insta")
            #calculate_variance.sh grid_sigma_Insta_states.dat
            #mv grid_sigma_Insta_states_var.rst $obs_set_dir/variance.grd
            ;;
        "Taver")
            calculate_variance.sh grid_sigma_Taver_states.dat
            mv grid_sigma_Taver_states_var.rst $obs_set_dir/variance.grd
            ;;
        *) error "Unknown A_mode $A_mode"
        ;;
    esac

    # Initialization
    cd $obs_set_dir
    cp $station_set station.tbl # Set observational network
    cp $DAS/obs/$obs_program .  # Copy observation program
    ln -fs $DAS/speedy/common/orography_t30.dat $obs_set_dir/fort.21  # Set surface geopotential
    touch create_observations.log

    # Cycle over nature run states
    cat $nature_dir/grid_sigma_Insta_states.dat | while read state; do 
        echo $state # or whaterver you want to do with the $line variable

echo $state
        # Run fortran code
        ln -fs $nature_dir/$state true.grd
        ./$obs_program >> create_observations.f90.log

        # Archive observation
        state_name="${state%.*}"
        mv obs.dat $obs_set_dir/$state_name.obs
        rm true.grd
    done
    
    cp $nature_dir/time_start.dat .
    cp $nature_dir/time_final.dat .
    
    # Cleaning up
    rm -f fort.21
    rm -f $obs_program
    
    echo "==============================================="
    echo " Observations succesfully created"
    echo "==============================================="
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

#create_observations "nature_6hr_100steps" "max_tree_network" "nature_6hr_100steps_obs" "$ARCHIVE_DIR" | tee tmp.log

create_observations $@
exit $?


#------------------------------------------
# Previous cycling mechanism

   #local present_time=$START_TIME
    
    ## Advance time
    #present_time=`time_increment $present_time ${assim_period}` 

    #nobs=0

    #while [ $present_time -le $FINAL_TIME ]
    #do  
        #echo ">>> present_time = $present_time"

        ## Set restart set to be observed
        #ln -fs ${NATURE_DIR}/${present_time}_TA.grd true.grd # Time averaged
##        ln -fs ${NATURE_DIR}/${present_time}.grd true.grd   # Instantaneous
        
        ## Harvest observation
        #if [ $verbose -ge 1 ]; then
           #./generate_obs.exe
##           ./obsmake.exe
        #else
           #./generate_obs.exe > /dev/null
##           ./obsmake.exe > /dev/null
        #fi

        ## Archive observation
        #mv obs.dat $OBS_ARCH_DIR/$present_time.obs
        #rm true.grd
        
        ## Advance time
        #present_time=`time_increment $present_time ${assim_period}` 
   
        #nobs=$((nobs + 1))    
    #done
      #echo "------------------------------------------"
    #echo "Number of model states observed = ${nobs}"    
    #echo "------------------------------------------"

    
   
