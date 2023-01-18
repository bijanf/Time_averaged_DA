spinup_model(){
    funct_opening always
    
    # Spinned-up states don't depend on time average lenght
    # so TA, AN restarts are not saved
    
    local SPINUP_START=$1
    local SPINUP_LENGHT=$2

    SPINUP_FINAL=`time_increment $SPINUP_START $SPINUP_LENGHT`

    echo "------------------------------"
    echo "SPINUP_START = $SPINUP_START"
    echo "SPINUP_LENGHT= $SPINUP_LENGHT"
    echo "SPINUP_FINAL = $SPINUP_FINAL"
    echo "------------------------------"
  
    if [ -d $SPINUP_DIR ]; then
        echo "There is already a spinup run called "$( basename "$SPINUP_DIR" )
        read -p "Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $SPINUP_DIR; mkdir -p $SPINUP_DIR;;
                 * ) return;;
        esac
    else
        mkdir -p $SPINUP_DIR    
    fi

    cd $TMP_DIR

    echo "-----------------------------------------------"
    echo " Performing a SPINUP_LENGHT long run starting "
    echo " from rest and writing a gridded restart file" 
    echo "-----------------------------------------------"
    
    #build_speedy
    #pwd
    #ls

    run_speedy 0 2 $SPINUP_START $SPINUP_LENGHT
    PRESENT_TIME=$(cat FORECAST_TIME.txt) 
    
    ls *.ctl
    cp ${PRESENT_TIME}.grd    $SPINUP_DIR 
    cp ${PRESENT_TIME}_p.grd  $SPINUP_DIR 
#    cp ${PRESENT_TIME}.ctl    $SPINUP_DIR
#    cp ${PRESENT_TIME}_p.ctl  $SPINUP_DIR
	echo $PRESENT_TIME  > ${SPINUP_DIR}/spinup_last_date.txt
    cd ..

    funct_closing always
}    

#> @brief Function to create the nature run
ta_create_nature_run(){
    funct_opening always
        
    create_archive_dir $NATURE_DIR || return 0
    local nature_length=$1
    
    #-----------------------------------------------------------
    # 1. Nature run time interval setting
    #   (Start from the end of spinup p)
    #-----------------------------------------------------------
    local START_TIME=$(cat ${SPINUP_DIR}/spinup_last_date.txt) 
    local FINAL_TIME=$(time_increment $START_TIME $nature_length)
    echo $START_TIME > ${NATURE_DIR}/nature_start_time.txt
    echo $FINAL_TIME > ${NATURE_DIR}/nature_final_time.txt
    
    echo "---------------------------"
    echo " START_TIME = $START_TIME" 
    echo " FINAL_TIME = $FINAL_TIME" 
    echo "---------------------------"

#    #---------------------------------
#    # Warn about too big model output
#    #---------------------------------
#    if [ $nature_length -ge 0000030000 ]; then
#      echo "More than 3 months of 6-hourly model ouput will go to archive!!"
#      read -p "Do you want to continue? [Yy]* / * " yn
#      case $yn in
#           [Yy]* ) ;;
#               * ) return;;
#      esac
#    fi

    #----------------------------------------------------
    # 2. Run model once from gridded instantaneous state
    #    to gridded inst. and T.A. decomposed states
    #----------------------------------------------------
    local present_time=$START_TIME

    cd $TMP_DIR # all the action takes place in TMP_DIR
    cp imp.exe ${NATURE_DIR}

    cd ${NATURE_DIR}
    set_boundary_conditions t30 .
    cp ${scripts_dir}/run_speedy.sh .
    
    cp ${SPINUP_DIR}/${present_time}.grd .
    forecast_time=$(time_increment $present_time $assim_period)
        
    if [ $verbose -ge 2 ]; then
       bash -x run_speedy.sh 2 2 $present_time $forecast_time .
    else
       bash    run_speedy.sh 2 2 $present_time $forecast_time .
    fi

    # Store descriptor files
    mv ${forecast_time}_TA.ctl yyyymmddhh_TA.ctl
    mv ${forecast_time}_AN.ctl yyyymmddhh_AN.ctl

    # Advance time
    present_time=$forecast_time

    #------------------------------------------------
    # 3. Produce the rest of the nature run 
    #------------------------------------------------
    while [ $present_time -lt $FINAL_TIME ]
    do
      echo ">>> present_time = $present_time"
      forecast_time=$(time_increment $present_time $assim_period)
        
      # Run model from gridded T.A. decomposed restart files
      # to gridded inst. and T.A. decomposed restart files
      if [ $verbose -ge 2 ]; then
        bash -x run_speedy.sh 3 2 $present_time $forecast_time .
      else
        bash    run_speedy.sh 3 2 $present_time $forecast_time .
      fi
 
      # Advance time
      present_time=$forecast_time
               
    done

    funct_closing always
}

ta_da_experiment(){
    funct_opening always
    
#    create_archive_dir $EXP_ARCH_DIR || return 0

#    ta_create_nature_run $nature_length        # Start from the end of spinup

    # Constrained run
#    rm -rf $OBS_ARCH_DIR

    ta_create_observations $obs_network # During the whole nature run length
    ta_assimilation_run $exp_length $ASSIM_RUN_DIR

#    # Free_run (no observations)
#    ta_create_observations "station_dummy.tbl" # During the whole nature run length
#    ta_assimilation_run $exp_length $FREE_RUN_DIR

    funct_closing always
}


ta_create_observations(){
    funct_opening always
    
    local station_network=$1
    
    create_archive_dir $OBS_ARCH_DIR || return 0

    #-----------------------------------------------------------
    # Time interval setting (taken from the nature run)
    #-----------------------------------------------------------
    local START_TIME=$(cat ${NATURE_DIR}/nature_start_time.txt)
    local FINAL_TIME=$(cat ${NATURE_DIR}/nature_final_time.txt)
    echo $START_TIME > ${OBS_ARCH_DIR}/obs_start_time.txt
    echo $FINAL_TIME > ${OBS_ARCH_DIR}/obs_final_time.txt

    echo "---------------------------"
    echo "START_TIME = $START_TIME" 
    echo "FINAL_TIME = $FINAL_TIME" 
    echo "---------------------------"
    
        calculate_variance
exit
    
    cd ${OBS_CODE_DIR}
    
    # Set surface geopotential
    ln -fs $SPEEDY/common/orography_t30.dat fort.21
    
    # Set observational network
    cp $station_network station.tbl
    
    local present_time=$START_TIME
    
    # Advance time
    present_time=`time_increment $present_time ${assim_period}` 

    nobs=0

    while [ $present_time -le $FINAL_TIME ]
    do  
        echo ">>> present_time = $present_time"

        # Set restart set to be observed
        ln -fs ${NATURE_DIR}/${present_time}_TA.grd true.grd # Time averaged
#        ln -fs ${NATURE_DIR}/${present_time}.grd true.grd   # Instantaneous
        
        # Harvest observation
        if [ $verbose -ge 1 ]; then
           ./generate_obs.exe
#           ./obsmake.exe
        else
           ./generate_obs.exe > /dev/null
#           ./obsmake.exe > /dev/null
        fi

        # Archive observation
        mv obs.dat $OBS_ARCH_DIR/$present_time.obs
        rm true.grd
        
        # Advance time
        present_time=`time_increment $present_time ${assim_period}` 
   
        nobs=$((nobs + 1))    
    done
    
    echo "------------------------------------------"
    echo "Number of model states observed = ${nobs}"    
    echo "------------------------------------------"
    
    rm fort.21    # Cleaning up
    
    funct_closing always
}

ta_assimilation_run(){
    funct_opening always

    local experiment_length=$1
    local  ensemble_run_dir=$2
    
    create_archive_dir $ensemble_run_dir || return 0

    #-----------------------------------------------------------
    # Max Time interval taken from the observation set
    #-----------------------------------------------------------
    local START_TIME=$(cat ${OBS_ARCH_DIR}/obs_start_time.txt)
    local   MAX_TIME=$(cat ${OBS_ARCH_DIR}/obs_final_time.txt)
    local FINAL_TIME=$(time_increment $START_TIME $experiment_length)
    
    if [ $FINAL_TIME -gt $MAX_TIME ]; then 
       error_exit "Requested Experiment longer than observations"
    fi

    echo "---------------------------"
    echo "START_TIME = $START_TIME" 
    echo "FINAL_TIME = $FINAL_TIME" 
    echo "---------------------------"
    
    #-----------------------------
    # Set up LETKF enviroment
    #-----------------------------

#    # Cleaning and creating work directory
#    if [ -d "$LETKF_TMP_DIR" ]; then
#      rm -rf ${LETKF_TMP_DIR}
#    fi
#    mkdir ${LETKF_TMP_DIR};
#    cd ${LETKF_TMP_DIR}

    create_ensemble_run_folder_structure $ensemble_run_dir

    cd $TMP_DIR

    # Copy and link needed fortran executables
    cp ${scripts_dir}/run_speedy.sh .
    ln -fs ${LETKF_CODE_DIR}/letkf.exe letkf.exe

    #------------------------------------
    # Setting up MPI (Machine dependent)
    #------------------------------------
    # Load appropriate MPI module (Machine dependent)
#    setup_modules_system
    #module load mpich2/gfortran-4.4.5/1.3.1
#    module load mpich2/ifort-12.0.2/1.3.1


    #cp ${HERE}/mpd.conf ~/.mpd.conf # Machine config file
    #chmod 600 ~/.mpd.conf
    #echo "Booting MPI";  mpdboot -n 1
    #mpdtrace

    #--------------------
    # Analysis_cycle
    #-------------------- 
    local PRESENT_TIME=$START_TIME
    ta_create_initial_ensemble $ensemble_run_dir $START_TIME
    
    for member in `seq $members`
    do
      member=`printf '%03d' ${member}`
      cp imp.exe ${ensemble_run_dir}/guess/$member
      set_boundary_conditions t30 ${ensemble_run_dir}/guess/$member
    done

    FORECAST_TIME=$(time_increment $PRESENT_TIME $assim_period)
    ta_forecasting_step 2 2 $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME
    PRESENT_TIME=$FORECAST_TIME # advance time

    while [ $PRESENT_TIME -le $FINAL_TIME ]
    do
        echo ">>> PRESENT_TIME = $PRESENT_TIME"

        ta_assimilation_step $ensemble_run_dir
        
        FORECAST_TIME=`time_increment $PRESENT_TIME ${assim_period}`

        ta_forecasting_step 3 2 $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME
                
        PRESENT_TIME=$FORECAST_TIME # advance time
    done
    
  	if [ $NOSAVEENS -eq 1 ]; then
       # Cleaning up ensemble members folders
       rm -rf ${ensemble_run_dir}/guess/???
       rm -rf ${ensemble_run_dir}/anal/???
       rm -rf ${ensemble_run_dir}/anal_f/???
    fi
    
    #-----------------
    # Closing MPI
    #-----------------
    mpdallexit
    
    funct_closing always
}


create_ensemble_run_folder_structure(){
    funct_opening
    
    local ensemble_run_dir=$1
    #===================================================================
    #   This script prepares the folder structure for a ensemble run
    #===================================================================
    mkdir -p ${ensemble_run_dir}/log
    mkdir -p ${ensemble_run_dir}/infl_mul
    cp $SPEEDY/common/yyyymmddhh*.ctl ${ensemble_run_dir}/infl_mul
    #
    # for each ensemble member, the mean and the spread create
    # anal, anal_f and guess folder and fill then with ctl files
    #
    for member in `seq $members`
    do
        member=`printf '%03d' ${member}`
        mkdir -p ${ensemble_run_dir}/anal/$member
        mkdir -p ${ensemble_run_dir}/anal_f/$member
        mkdir -p ${ensemble_run_dir}/guess/$member
    done
    
    for member in mean sprd
    do
        mkdir -p ${ensemble_run_dir}/anal/$member
        mkdir -p ${ensemble_run_dir}/anal_f/$member
        mkdir -p ${ensemble_run_dir}/guess/$member
    done
    
    funct_closing
}


ta_create_initial_ensemble(){
    funct_opening
    
    # Create Initial ensemble for instant INITIAL_TIME

    local ensemble_run_dir=$1
    local     initial_time=$2

    # Sampling nature run every 12 hours starting from initial_time
    local sample_time=$initial_time

    # Advance time $assim_period (first nature run state is instantaneous!)
    sample_time=`time_increment $sample_time $assim_period`
#    sample_time=`time_increment $sample_time $assim_period`
    
    for member in `seq $members`
    do
      member=`printf '%03d' ${member}`
      cp ${NATURE_DIR}/${sample_time}_TA.grd ${ensemble_run_dir}/guess/${member}/${initial_time}.grd
#      cp ${NATURE_DIR}/${sample_time}_TA.grd ${ensemble_run_dir}/guess/${member}/${initial_time}_TA.grd
#      cp ${NATURE_DIR}/${sample_time}_AN.grd ${ensemble_run_dir}/guess/${member}/${initial_time}_AN.grd
      
      # Advance time $assim_period
      sample_time=`time_increment $sample_time $assim_period`
      
      cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/anal/${member}
      cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/anal_f/${member}
      cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/guess/${member}
      cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/anal/${member}
      cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/anal_f/${member}
      cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/guess/${member}
    done

    for member in mean sprd
    do
        cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/anal/${member}
        cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/anal_f/${member}
        cp ${NATURE_DIR}/yyyymmddhh_TA.ctl ${ensemble_run_dir}/guess/${member}
        cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/anal/${member}
        cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/anal_f/${member}
        cp ${NATURE_DIR}/yyyymmddhh_AN.ctl ${ensemble_run_dir}/guess/${member}
    done
    
    funct_closing
}


ta_assimilation_step(){
    funct_opening

    local ensemble_run_dir=$1
    #---------------------------------------
    # Linking input files
    #---------------------------------------
    # Guess fields
    for member in `seq $members`
    do
      member=`printf '%03d' ${member}`
#      ln -fs ${ensemble_run_dir}/guess/${member}/${PRESENT_TIME}.grd gs01${member}.grd
      ln -fs ${ensemble_run_dir}/guess/${member}/${PRESENT_TIME}_TA.grd gs01${member}.grd
    done
    
    # Adaptive Inflation factor
    if [ -f ${ensemble_run_dir}/infl_mul/${PRESENT_TIME}.grd ]; then
       ln -fs ${ensemble_run_dir}/infl_mul/${PRESENT_TIME}.grd infl_mul.grd
    fi
    
    # Observations
    ln -fs $OBS_ARCH_DIR/${PRESENT_TIME}.obs obs01.dat
    
    # Surface geopotential height
    ln -fs $SPEEDY/common/orography_t30.dat fort.21
    
    #-------------------------------------------------------------
    # Run Miyoshi's parallel LETKF program
    # ------------------------------------------------------------
    # To run system in background mpiexec must be redirected
    # from /dev/null (< /dev/null), otherwise process crashes:
    # "mpiexec_tux04 (handle_stdin_input 1089): stdin problem;
    #  if pgm is run in background, redirect from /dev/null
    #  e.g.: mpiexec -n 4 a.out < /dev/null & "
    #-------------------------------------------------------------
    mpiexec -n $NODES ./letkf.exe < /dev/null

    if [ $verbose -ge 1 ]; then
         tail -n 9 NOUT-000 # show observational departure
#         ls *NOUT*
#         tail -n 9 NOUT-000 # show observational departure
    fi

    #---------------------------------------
    # Archive mean and spread fields
    #---------------------------------------
    mv NOUT-000 ${ensemble_run_dir}/log/${PRESENT_TIME}_rank-000.log
    mv NOUT-001 ${ensemble_run_dir}/log/${PRESENT_TIME}_rank-001.log
    mv NOUT-002 ${ensemble_run_dir}/log/${PRESENT_TIME}_rank-002.log
    mv NOUT-003 ${ensemble_run_dir}/log/${PRESENT_TIME}_rank-003.log

    if [ -f infl_mul.grd ]; then
	    cp infl_mul.grd ${ensemble_run_dir}/infl_mul/${PRESENT_TIME}.grd
    fi
    mv gues_me.grd ${ensemble_run_dir}/guess/mean/${PRESENT_TIME}_TA.grd
    mv gues_sp.grd ${ensemble_run_dir}/guess/sprd/${PRESENT_TIME}_TA.grd
    mv anal_me.grd ${ensemble_run_dir}/anal/mean/${PRESENT_TIME}_TA.grd
    mv anal_sp.grd ${ensemble_run_dir}/anal/sprd/${PRESENT_TIME}_TA.grd
    
    #---------------------------------------
    # Archive analysis fields
    #---------------------------------------
    for member in `seq $members`
    do
	    member=`printf '%03d' ${member}`
	    mv anal${member}.grd ${ensemble_run_dir}/anal/${member}/${PRESENT_TIME}_TA.grd
    done

    funct_closing
}

ta_forecasting_step(){
    funct_opening
    #===========================
	# ensemble prediction
    #===========================
    
    local           istart=$1
    local           jstart=$2
    local ensemble_run_dir=$3
    local     PRESENT_TIME=$4
    local    FORECAST_TIME=$5

#	cd $LETKF_TMP_DIR

    # Estani's version:
    #seq 1 $members | xargs -n1 -P$NODE -Imember bash -x ensfcst.sh $SPEEDY ${ensemble_run_dir} ${PRESENT_TIME} $FORECAST_TIME member $N

	member=1
	while [ $member -le $members ]
	do
        N=1
        while test $N -le $NODES
        do
            N=`printf '%02d' ${N}`
            member_id=`printf '%03d' ${member}`
            echo "Member $member_id running in processor $N"

            # Initialize processor folder
            #rm -rf $processor; mkdir $processor; cp imp.exe $processor; cd $processor
        
            input=${ensemble_run_dir}/guess/${member_id}
            bash run_speedy.sh $istart $jstart $PRESENT_TIME $FORECAST_TIME $input &
            (( member++ ))
            (( N++ ))

            # Older interfaces        
            #output=${ensemble_run_dir}/guess/${member}
            #bash run_speedy.sh $istart $jstart $PRESENT_TIME $FORECAST_TIME $input $output

            # bash -x $istart $jstart run_speedy.sh $SPEEDY $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME $member $N &

            # if   [ $verbose -ge 2 ]; then
            #   echo $'\n'"Running ta_run_speedy.sh"$'\n'
            #   bash -x ta_run_speedy.sh $SPEEDY $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME $member $N &
            # elif [ $verbose -ge 1 ]; then 
            #   bash ta_run_speedy.sh $SPEEDY $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME $member $N &
            # else
            #   bash ta_run_speedy.sh $SPEEDY $ensemble_run_dir $PRESENT_TIME $FORECAST_TIME $member $N > /dev/null &
            # fi
        done
        wait # wait for the threads to finish
	done

    #-------------
	# Clean up
    #-------------
	if [ $NOSAVEENS -eq 1 ]; then
      for member in `seq $members`
      do
        member=`printf '%03d' ${member}`

#        rm -f ${ensemble_run_dir}/guess/${member}/${PRESENT_TIME}.grd
#        rm -f ${ensemble_run_dir}/guess/${member}/${PRESENT_TIME}_p.grd
#        rm -f ${ensemble_run_dir}/anal/${member}/${PRESENT_TIME}.grd
        rm -f ${ensemble_run_dir}/guess/${member}/${PRESENT_TIME}*.grd
        rm -f ${ensemble_run_dir}/anal/${member}/${PRESENT_TIME}*.grd
#        rm -f ${ensemble_run_dir}/anal_f/${member}/${PRESENT_TIME}.grd
#        rm -f ${ensemble_run_dir}/anal_f/${member}/${PRESENT_TIME}_p.grd
      done
	fi
    
    funct_closing
}

create_station_table(){
    cd ${OBS_CODE_DIR}
    bash generate_max_tree_network.sh
}

