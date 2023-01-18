
ta_free_run(){
    funct_opening always

    create_archive_dir $FREE_RUN_DIR || return 0
    
    #-----------------------------------------------------------
    # Max Time interval taken from the observation set
    #-----------------------------------------------------------
    local experiment_length=$1

    START_TIME=$(cat ${OBS_ARCH_DIR}/obs_start_time.txt)
      MAX_TIME=$(cat ${OBS_ARCH_DIR}/obs_final_time.txt)
    FINAL_TIME=$(time_increment $START_TIME $experiment_length)
    
    if [ $FINAL_TIME -gt $MAX_TIME ]; then 
       error_exit "Requested Experiment longer than observations"
    fi

    if [ $verbose -ge 0 ]; then
      echo "---------------------------"
      echo "START_TIME = $START_TIME" 
      echo "FINAL_TIME = $FINAL_TIME" 
      echo "---------------------------"
    fi
    
    create_ensemble_run_folder_structure $FREE_RUN_DIR
    cd $TMP_DIR
    
    # Copy, link or Create needed executables
    build_speedy
    cp ${scripts_dir}/ta_run_speedy.sh .

    #--------------------
    # Analysis_cycle
    #-------------------- 
    PRESENT_TIME=$START_TIME
    ta_create_initial_ensemble $FREE_RUN_DIR $START_TIME

    while [ $PRESENT_TIME -le $FINAL_TIME ]
    do
      echo ">>> PRESENT_TIME = $PRESENT_TIME"

      # Dummy identity assimilation_step
      for member in `seq $members`
      do
        member=`printf '%03d' ${member}`

        # GRIDDED time average state
        ln -fs ${FREE_RUN_DIR}/guess/${member}/${PRESENT_TIME}_TA.grd ${FREE_RUN_DIR}/anal/${member}/${PRESENT_TIME}_TA.grd
        # GRIDDED time average anomaly
        ln -fs ${FREE_RUN_DIR}/guess/${member}/${PRESENT_TIME}_TA.grd ${FREE_RUN_DIR}/anal/${member}/${PRESENT_TIME}_TA.grd
      done
      
      FORECAST_TIME=`time_increment $PRESENT_TIME ${assim_period}`

      ta_forecasting_step $FREE_RUN_DIR
       
      PRESENT_TIME=$FORECAST_TIME # advance time
      
    done
    
    # Cleaning up assimilation_run files which don't apply to a free run
    rm -rf ${FREE_RUN_DIR}/anal
    rm -rf ${FREE_RUN_DIR}/anal_f
    rm -rf ${FREE_RUN_DIR}/infl_mul
    rm -rf ${FREE_RUN_DIR}/log
    
    funct_closing always
}
