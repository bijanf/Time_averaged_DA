create_nature_run(){
    echo "******************************************"
    echo " ${FUNCNAME} START"
    echo "******************************************"
    
    #-----------------------------------------------------------
    # Time interval setting
    #-----------------------------------------------------------
    local NATURE_LENGTH=$1
    START_TIME=$(cat ${SPINUP_DIR}/spinup_last_date.txt) # Start from the end of spinup 
    FINAL_TIME=$(time_increment $START_TIME $NATURE_LENGTH)
    echo "---------------------------"
    echo "START_TIME = $START_TIME" 
    echo "FINAL_TIME = $FINAL_TIME" 
    echo "---------------------------"

    #--------------------------------
    # Create or replace OBS_ARCH_DIR
    #--------------------------------
    if [ -d $NATURE_DIR ]; then
        echo "There is already a Nature run called "$( basename "$NATURE_DIR" )
        read -p "Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $NATURE_DIR; mkdir -p $NATURE_DIR;;
                 * ) return;;
        esac
    else
        mkdir -p $NATURE_DIR
    fi

    #---------------------------------
    # Warn about too big model output
    #---------------------------------
    if [ $NATURE_LENGTH -ge 0000030000 ]; then
        echo "More than 3 months of 6-hourly model ouput will go to archive!!"
        read -p "Do you want to continue? [Yy]*/* " yn
        case $yn in
             [Yy]* ) ;;
                 * ) return;;
        esac
    fi

    cd $TMP_DIR          # all the action takes place in TMP_DIR
    cp ${SPINUP_DIR}/* $NATURE_DIR
#    set_cls_instep 1 0  # generate config file for 6-hr run
    build_speedy
    
    #--------------------------------------
    # Run speedy and archive restart files 
    #--------------------------------------
    ### MAIN LOOP ###
    PRESENT_TIME=$START_TIME
    while test $PRESENT_TIME -lt $FINAL_TIME
    do
        ### run
        ln -fs ${NATURE_DIR}/${PRESENT_TIME}.grd fort.90 # set initial conditions
        run_speedy 2 2 ${PRESENT_TIME} 0000000006 #(from gridded to gridded restart files)
        PRESENT_TIME=$(cat FORECAST_TIME.txt) 
        
        ### store output
        mv ${PRESENT_TIME}*.grd $NATURE_DIR
    done
    echo $START_TIME > ${NATURE_DIR}/nature_start_time.txt
    echo $FINAL_TIME > ${NATURE_DIR}/nature_final_time.txt

    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"

}

create_observations(){
    echo "******************************************"
    echo " ${FUNCNAME} START"
    echo "******************************************"

    #-----------------------------------------------------------
    # Time interval setting (taken from the nature run)
    #-----------------------------------------------------------
    START_TIME=$(cat ${NATURE_DIR}/nature_start_time.txt)
    FINAL_TIME=$(cat ${NATURE_DIR}/nature_final_time.txt)
    echo "---------------------------"
    echo "START_TIME = $START_TIME" 
    echo "FINAL_TIME = $FINAL_TIME" 
    echo "---------------------------"

    #--------------------------------
    # Create or replace OBS_ARCH_DIR
    #--------------------------------
    if [ -d $OBS_ARCH_DIR ]; then
        echo "There is already a observation set called "$( basename "$OBS_ARCH_DIR" )
        read -p "Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $OBS_ARCH_DIR; mkdir -p $OBS_ARCH_DIR;;
                 * ) return;;
        esac
    else
        mkdir -p $OBS_ARCH_DIR
    fi

    #-----------------------------
    # Build observation program
    #-----------------------------
    echo '>>>'
    echo '>>> ORSERVATION PROGRAM BUILDING BEGINNED'
    cd ${OBS_CODE_DIR}
#    cp ${OBS_CODE_DIR}/* .  # bring obs. code to tmpdir
    cp make_obsmake_gfortran.sh make_obsmake.sh
    case $verbose in
         1) bash -x make_obsmake.sh;;
         0) bash make_obsmake.sh > /dev/null;;
    esac
    echo '>>> ORSERVATION PROGRAM BUILDING FINISHED'
    echo '>>>'

    #-------------------------------------
    # Create observation set 
    #------------------------------------- 
    ln -fs $SPEEDY/common/orography_t30.dat fort.21
    
    ### MAIN LOOP ###
    PRESENT_TIME=$START_TIME
    nobs=0
    while [ $PRESENT_TIME -le $FINAL_TIME ]
    do  
        ln -fs $NATURE_DIR/$PRESENT_TIME.grd true.grd
        case $verbose in
             1) ./obsmake.exe;;
             0) ./obsmake.exe > /dev/null;;
        esac
        mv obs.dat $OBS_ARCH_DIR/$PRESENT_TIME.obs
        rm true.grd
        PRESENT_TIME=`time_increment $PRESENT_TIME 0000000006` # Advance time 6 hours
        nobs=$((nobs + 1))    
    done
    echo "Number of observations = ${nobs}"    
    rm fort.21
    echo $START_TIME > ${OBS_ARCH_DIR}/obs_start_time.txt
    echo $FINAL_TIME > ${OBS_ARCH_DIR}/obs_final_time.txt
    
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"
}

assimilation_run(){
    echo "******************************************"
    echo " ${FUNCNAME} START"
    echo "******************************************"
    #=======================================================================
    #  This function runs the SPEEDY-LETKF cycle in parallel computing environment
    #=======================================================================

    #-----------------------------------------------------------
    # Time interval setting (taken from the observation set)
    #-----------------------------------------------------------
    local EXPERIMENT_LENGTH=$1

    START_TIME=$(cat ${OBS_ARCH_DIR}/obs_start_time.txt)
      MAX_TIME=$(cat ${OBS_ARCH_DIR}/obs_final_time.txt)
    FINAL_TIME=$(time_increment $START_TIME $EXPERIMENT_LENGTH)
    
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
    build_LETKF
    create_ensemble_folder_structure
    copy_initial_conditions $START_TIME
    # Creating work directory
    mkdir -p $LETKF_TMP_DIR; cd $LETKF_TMP_DIR
    #rm -rf $TMP_DIR; mkdir -p $TMP_DIR/letkf; cd $TMP_DIR/letkf
    #	rm -rf $TMP_DIR/letkf
    cp ${HERE}/scripts/da_ensfcst.sh .
    ln -s $LETKF_CODE_DIR/$LETKF $LETKF

    #-------------------------
    # Setting up MPI stuff
    #-------------------------
    cp ${HERE}/mpd.conf ~/.mpd.conf
    chmod 600 ~/.mpd.conf
    #### mpd
    echo '>>> mpdboot'
    mpdboot -n 1
    mpdtrace

    #------------------------------------------
    # Create speedy executable for 6 hour runs
    #------------------------------------------
    #set_cls_instep 1 0  # generate config file for 6-hr run
    build_speedy

    #--------------------
    # Analysis_cycle
    #-------------------- 
    PRESENT_TIME=$START_TIME
    while [ $PRESENT_TIME -le $FINAL_TIME ]
    do
        echo ">>> PRESENT_TIME = $PRESENT_TIME"
        
        if  [ $verbose -ge 1 ]; then
            assimilation_step
        else
            assimilation_step>/dev/null 2>&1
        fi
        
        FORECAST_TIME=`time_increment $PRESENT_TIME 0000000006`

        if  [ $verbose -ge 1 ]; then
            forecasting_step
        else
            forecasting_step>/dev/null 2>&1
        fi
        
        PRESENT_TIME=$FORECAST_TIME # advance time
    done
    
    mpdallexit
    echo "******************************************"
    echo " ${FUNCNAME} NORMAL END"
    echo "******************************************"
}     

da_experiment(){
    #--------------------------------
    # Create or replace EXP_ARCH_DIR
    #--------------------------------
    if [ -d $EXP_ARCH_DIR ]; then
        echo "There is already an experiment called "$( basename "$EXP_ARCH_DIR" )
        read -p "Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $EXP_ARCH_DIR; mkdir -p $EXP_ARCH_DIR;;
                 * ) return;;
        esac
    else
        mkdir -p $EXP_ARCH_DIR
    fi

    create_nature_run $NATURE_LENGTH # Start from the end of spinup
    create_observations             # During the whole nature run length
    assimilation_run $EXP_LENGTH
}

