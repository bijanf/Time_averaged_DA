
create_ensemble_folder_structure(){
    funct_opening
        
    #===================================================================
    #   This script prepares the folder structure of the filter
    #===================================================================
    mkdir -p ${ASSIM_RUN_DIR}/log
    mkdir -p ${ASSIM_RUN_DIR}/infl_mul
    cp $SPEEDY/common/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/infl_mul
    #
    # for each ensemble member, the mean and the spread create
    # anal, anal_f and guess folder and fill then with ctl files
    #
    for member in `seq $members`
    do
        member=`printf '%03d' ${member}`
        mkdir -p ${ASSIM_RUN_DIR}/anal/${member}
        mkdir -p ${ASSIM_RUN_DIR}/anal_f/${member}
        mkdir -p ${ASSIM_RUN_DIR}/guess/${member}
    done
    
    for member in mean sprd
    do
        mkdir -p ${ASSIM_RUN_DIR}/anal/${member}
        mkdir -p ${ASSIM_RUN_DIR}/anal_f/${member}
        mkdir -p ${ASSIM_RUN_DIR}/guess/${member}
    done
    
    funct_closing
}

copy_initial_conditions(){
    echo ">>>"
    echo ">>> ${FUNCNAME} STARTED"

    # Create Initial ensemble for instant INITIAL_TIME
    local INITIAL_TIME=$1

    # Sampling nature run every 12 hours starting from START_TIME
    PRESENT_TIME=$START_TIME
    for member in `seq $members`
    do
        member=`printf '%03d' ${member}`
        cp $NATURE_DIR/$PRESENT_TIME.grd ${ASSIM_RUN_DIR}/guess/${member}/$INITIAL_TIME.grd
        PRESENT_TIME=`time_increment $PRESENT_TIME 0000000012` # Advance time 12 hours
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/anal/${member}
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/anal_f/${member}
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/guess/${member}
    done
        for member in mean sprd
    do
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/anal/${member}
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/anal_f/${member}
        cp ${NATURE_DIR}/yyyymmddhh*.ctl ${ASSIM_RUN_DIR}/guess/${member}
    done

    echo ">>> ${FUNCNAME} NORMAL END"
    echo ">>>"
}

assimilation_step(){
    echo ">>>"
    echo ">>> ${FUNCNAME} STARTED"

    #---------------------------------------
    # Linking input files
    #---------------------------------------
    # Guess fields
    for member in `seq $members`
    do
      member=`printf '%03d' ${member}`
      ln -fs ${ASSIM_RUN_DIR}/guess/${member}/$PRESENT_TIME.grd gs01${member}.grd
    done
    # Adaptive Inflation factor?
    if [ -f ${ASSIM_RUN_DIR}/infl_mul/$PRESENT_TIME.grd ]; then
       ln -fs ${ASSIM_RUN_DIR}/infl_mul/$PRESENT_TIME.grd infl_mul.grd
    fi
    # Observations
    ln -fs $OBS_ARCH_DIR/$PRESENT_TIME.obs obs01.dat
    # Surface geopotential height
    ln -fs $SPEEDY/common/orography_t30.dat fort.21
    
    #---------------------------------------
    # Run Miyoshi's parallel LETKF program
    #---------------------------------------
    echo ">>>"
    echo ">>> LETKF ASSIMILATION START"
    mpiexec -n $NODES ./$LETKF > /dev/null
    tail -n 9 NOUT-000
    #cat NOUT-000
    echo ">>> LETKF ASSIMILATION NORMAL END"
    echo ">>>"

    #---------------------------------------
    # Archive mean and spread fields
    #---------------------------------------
    mv NOUT-000 ${ASSIM_RUN_DIR}/log/$PRESENT_TIME.log
    if [ -f infl_mul.grd ]; then
	    cp infl_mul.grd ${ASSIM_RUN_DIR}/infl_mul/$PRESENT_TIME.grd
    fi
    mv guess_me.grd ${ASSIM_RUN_DIR}/guess/mean/$PRESENT_TIME.grd
    mv guess_sp.grd ${ASSIM_RUN_DIR}/guess/sprd/$PRESENT_TIME.grd
    mv anal_me.grd ${ASSIM_RUN_DIR}/anal/mean/$PRESENT_TIME.grd
    mv anal_sp.grd ${ASSIM_RUN_DIR}/anal/sprd/$PRESENT_TIME.grd
    
    #---------------------------------------
    # Archive analysis fields
    #---------------------------------------
    for member in `seq $members`
    do
	    member=`printf '%03d' ${member}`
	    mv anal${member}.grd ${ASSIM_RUN_DIR}/anal/${member}/$PRESENT_TIME.grd
    done

    echo ">>> ${FUNCNAME} NORMAL END"
    echo ">>>"
}

forecasting_step(){
    echo ">>>"
    echo ">>> ${FUNCNAME} STARTED"
	# ensemble prediction

#	cd $LETKF_TMP_DIR

    # Estani's version:
    #seq 1 $members | xargs -n1 -P$NODE -Imember bash -x da_ensfcst.sh $SPEEDY ${ASSIM_RUN_DIR} $PRESENT_TIME $FORECAST_TIME member $N

	member=1
	while test $member -le $members
	do
		N=1
        while test $N -le $NODES
        do
            N=`printf '%02d' ${N}`
            member=`printf '%03d' ${member}`
            echo "member $member in NODE $N"
            echo $'\n'"Running da_ensfcst.sh"$'\n'
            if   [ $verbose -ge 1 ]; then
                bash -x da_ensfcst.sh $SPEEDY ${ASSIM_RUN_DIR} $PRESENT_TIME $FORECAST_TIME $member $N &
            else
                bash    da_ensfcst.sh $SPEEDY ${ASSIM_RUN_DIR} $PRESENT_TIME $FORECAST_TIME $member $N &
            fi
            
            member=`expr $member + 1`
            N=`expr  $N + 1`
        done
        ### wait for the end of parallel processing
        time wait
	done
	#
	# Clean up
	#
	if test $NOSAVEENS -eq 1
	then
        for member in `seq $members`
        do
            member=`printf '%03d' ${member}`
            rm -f ${ASSIM_RUN_DIR}/guess/${member}/${PRESENT_TIME}.grd
            rm -f ${ASSIM_RUN_DIR}/guess/${member}/${PRESENT_TIME}_p.grd
            rm -f ${ASSIM_RUN_DIR}/anal/${member}/${PRESENT_TIME}.grd
            rm -f ${ASSIM_RUN_DIR}/anal_f/${member}/${PRESENT_TIME}.grd
            rm -f ${ASSIM_RUN_DIR}/anal_f/${member}/${PRESENT_TIME}_p.grd
        done
	fi
    echo ">>> ${FUNCNAME} NORMAL END"
    echo ">>>"
}

