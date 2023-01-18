#!/usr/bin/env bash
#set -x
#verbose=1
#=======================================================================
#> @brief Propagate an ensemble of SPEEDY states
#=======================================================================
ensemble_run(){
    [[ $# -eq 8 ]] || error "Usage: ensemble_run run_name nature_name station_set_name sampling_name obs_config A_mode members save_members"
    run_name=$1
    nature_name=$2
    station_set_name=$3
    sampling_name=$4
    obs_config=$5
    A_mode=$6
    members=$7
    save_members=$8

    echo "==============================================="
    echo " Ensemble Run"
    echo "==============================================="

    storing_dir=$ARCHIVE_DIR
    nature_dir=${storing_dir}/${nature_name}
    ens_run_dir=${storing_dir}/${run_name}
    sampling_dir=${storing_dir}/${sampling_name}
    station_set=${STATIONS}/${station_set_name}.tbl

    echo " - Parsing Arguments"

    [ -d   $nature_dir ] || error "There is no nature dataset '$nature_name' in $storing_dir"
    [ -d $sampling_dir ] || error "There is no sampling dataset '$sampling_name' in $storing_dir"
    [ -f $station_set ]  || error "There is no station_set table '$station_set_name' in $STATIONS"
    case "$A_mode" in
        "Free"|"Insta"|"Taver") ;;
        *) error "Unknown Analysis mode";;
    esac

    echo " - Initializing the ensemble run"    ; ensemble_run_initialization

    echo " - Observing nature run $nature_name"; observe_nature #> /dev/null

    echo " - Selecting initial ensemble"       ; select_initial_states

    echo " - Analysis cycle"

    time_present=$time_zero; echo "   > time_present = $time_present"
    while [ $time_present -lt $time_final ]; do
        analysis_step # > /dev/null
        echo "   > time_present = $time_present"
    done

    echo " - Saving metainformation"; save_metainfo

    echo " - Cleaning up"           ; clean_dataset

    #echo " - Closing MPI"           ; mpdallexit

    echo "==============================================="
    echo " Ensemble Run ended successfully"
    echo "==============================================="
    return 0
}

#=======================================================================
#> @brief Harvests observations from a nature run
#=======================================================================
observe_nature(){

    echo "============================================="
    echo "   Observation step"
    echo "============================================="

    cd $ens_run_dir/obs

    echo " - Setting A_mode $A_mode"
    case $A_mode in
        "Insta") nature_states="grid_sigma_Insta_states" ;;
        "Taver") nature_states="grid_sigma_Taver_states" ;;
    esac

    echo " - Setting obs_config $obs_config"
    case $obs_config in
        "Origi" ) # Default Speedy-Letkf configuration
            obs_program="obsmake.exe"
            cp $DAS/obs/obserr.tbl .
            cd $nature_dir/states
            # Variance can be calculated just once
            calculate_variance.exe "${nature_states}.dat" "variance.grd"
#            calculate_variance.sh "${nature_states}.dat" "variance.rst"
            mv variance.grd $ens_run_dir/obs/
            ;;
        "STemp") # Only surface temperature measurements
            obs_program="create_observations.exe"
            cd $nature_dir/states
            calculate_variance.exe "${nature_states}.dat" "variance.grd"
#            calculate_variance.sh "${nature_states}.dat" "variance.rst"
            mv variance.grd $ens_run_dir/obs/
            #mv "${nature_states}_var.ctl" $ens_run_dir/obs/variance.ctl
            ;;
        *) error "Unknown obs_config $obs_config";;
    esac


    #echo "Initialization"
    cd $ens_run_dir/obs
    echo " - Copying observational network ${station_set_name}.tbl"
    cp $station_set station.tbl
    cp $DAS/obs/$obs_program .                           # Copy observation program
    ln -fs $DAS/speedy/common/orography_t30.dat fort.21  # Set surface geopotential

    echo " - Cycling over nature run states"
    cat $nature_dir/states/"${nature_states}.dat" | while read state; do

        echo "------------------------------------------------"
        echo "   nature run state = $state"
        echo "------------------------------------------------"
        # Run fortran code
        rm -f true.grd
        ln -s $nature_dir/states/$state
        case $obs_config in
            "Origi") # Default Speedy-Letkf configuration
                mv $state true.grd
                ./$obs_program
                rm true.grd
                ;;
            "STemp") # Only surface temperature measurements
                ./$obs_program $state $SNR
                rm $state
                ;;
        esac

        # Archive observation
        state_name="${state%.*}"
        mv obs.dat $state_name.obs
        echo "------------------------------------------------"

    done

    echo " - Cleaning up"
    rm -f fort.21
    rm -f $obs_program
}

#=======================================================================
#> @brief Prepares everyting for a ensemble run
#=======================================================================
ensemble_run_initialization(){
    create_archive_dir.sh $ens_run_dir || return 1

     # Set Time interval according to nature run
    cd $nature_dir
    [ -f dataset_complete.dat ] || error "Nature Run dataset is not complete"
    time_zero=$(cat time_zero.dat)
    time_start=$(cat time_start.dat)
    time_final=$(cat time_final.dat)
    time_step=$(cat time_step.dat)
    steps=$(cat steps.dat)
    echo " ---------------------------"
    echo "  time_start = $time_start"
    echo "  time_final = $time_final"
    echo " ---------------------------"

    # create array of members labels with 3 digits
    for member in $(seq $members); do
        smembers[member]=$(printf '%03d' ${member})
    done

    cd $ens_run_dir

    # create ensemble member folders
    for member in ${smembers[@]}; do
        # create prior, postr and pos_F folders
        mkdir -p prior/$member
        mkdir -p postr/$member
        #mkdir -p pos_F/$member

        # copy ctl files
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Insta.ctl prior/${member}
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Insta.ctl postr/${member}
        #cp $tmp_path/YYYYMMDDHH_grid_sigma.ctl pos_F/${member}

        # Copy SPEEDY executable and running wrapper
        cp $SPEEDY/model/tmp/$dy1_pgm   postr/$member
        cp $SPEEDY/$dy2_pgm             postr/$member

        # Link boundary conditions
        speedy_link_forcings.sh t30 postr/$member
    done

    # create mean and spread folders
    for member in mean spread; do
        # create prior, postr and pos_F folders
        mkdir -p prior/$member
        mkdir -p postr/$member
        #mkdir -p pos_F/$member

        # copy ctl files
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Insta.ctl prior/${member}
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Insta.ctl postr/${member}
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Taver.ctl prior/${member}
        cp $nature_dir/states/YYYYMMDDHH_grid_sigma_Taver.ctl postr/${member}
        #cp $tmp_path/YYYYMMDDHH_grid_sigma.ctl pos_F/${member}
    done

#    cp $DAS/tools/mean.exe prior/
#    cp $DAS/tools/mean.exe prior/

    mkdir -p infl_mul                              # creates inflation folder
#    cp $SPEEDY/common/YYYYMMDDHH_grid_sigma.ctl ${ens_run_dir}/infl_mul #put a ctl file there
    cp $LETKF/$da_pgm .                            # Copy LETKF executable
    ln -s $SPEEDY/common/orography_t30.dat fort.21 # Link Surface geopotential height
    mkdir -p obs                                   # Create log folder
    mkdir -p log                                   # Create log folder
}

#=======================================================================
#> @brief Gathers the initial states from a sampling dataset
#=======================================================================
select_initial_states(){
    member=0
    cat $sampling_dir/states/grid_sigma_Insta_states.dat | while read state; do
        member=$((member + 1))

        smember=$(printf '%03d' ${member})
        cp $sampling_dir/states/${state} $ens_run_dir/postr/$smember/${time_zero}_grid_sigma_Insta.rst
        #case "$A_mode" in
            #"Free"|"Insta")
                #cp $sampling_dir/${state} $ens_run_dir/postr/$smember/${time_zero}_grid_sigma.rst
                #;;
            #"Taver"       )
                #cp $sampling_dir/${state} $ens_run_dir/postr/$smember/${time_zero}_grid_sigma_Taver.rst
                #cp $DAS/tools/null_speedy_state_grid_sigma.rst $ens_run_dir/postr/$smember/${time_zero}_grid_sigma_Tanom.rst
                #;;
        #esac
        [ $member -lt $members ] || return 0
#        cp $sampling_dir/${state} $ens_run_dir/prior/$smember/${time_start}_grid_sigma.rst
    done
    return 0
}

#=======================================================================
#> @brief Core of the ensemble run
#=======================================================================
analysis_step(){
    # Propagate ensemble
    case "$A_mode" in
        "Insta") forecasting_step 2 3;;
        "Taver") forecasting_step 2 4;;
    esac
#if [ $time_present -eq $time_zero ]; then
        ##echo "   Forecasting Step zero"
        ## Special because it always starts from instantaneous states
        #case "$A_mode" in
            #"Insta") forecasting_step 2 3;;
            #"Taver") forecasting_step 2 4;;
        #esac
    #else
        #case "$A_mode" in
            #"Insta") forecasting_step 2 3;;
            #"Taver") forecasting_step 2 4;;
##            "Taver") forecasting_step 3 4;;
        #esac
    #fi

    # Link Observations
    ln -fs ${ens_run_dir}/obs/${time_present}_grid_sigma_${A_mode}.obs ${ens_run_dir}/obs01.dat
    #case $A_mode in
    #"Insta") ln -fs ${ens_run_dir}/obs/${time_present}_grid_sigma_Insta.obs ${ens_run_dir}/obs01.dat;;
    #"Taver") ln -fs ${ens_run_dir}/obs/${time_present}_grid_sigma_Taver.obs ${ens_run_dir}/obs01.dat;;
    #esac

    # Update ensemble
    assimilation_step

    if [ $save_members == "no" ];then
        # Clean up member states so as to keep dataset small
        for member in ${smembers[@]}; do
            rm -f ${ens_run_dir}/prior/${member}/${time_present}*.grd
            rm -f ${ens_run_dir}/postr/${member}/${time_present}*.grd
        done
    fi
}

#=======================================================================
#> @brief Propagates ensemble
#=======================================================================
forecasting_step(){
    [[ $# -eq 2 ]] || error "Usage: forecasting_step in_flag out_flag"
    local  in_flag=$1
    local out_flag=$2

    time_forecast=$(time_increment.exe $time_present $time_step)

    echo "============================================="
    echo "   Forecasting step"
    echo "============================================="

    member=1
    while [ $member -le $members ]; do
        N=1
        while [ $N -le $CORES ]; do
            member_id=$(printf '%03d' ${member})
            N=$(printf '%02d' ${N})
            echo "Member $member_id running in processor $N"

            cd ${ens_run_dir}/postr/${member_id}
            out_dir=${ens_run_dir}/prior/${member_id}
            ./speedy_run.sh $in_flag $out_flag $time_present $time_step $out_dir > /dev/null &

            (( member++ ))
            (( N++ ))
        done
        wait # wait for the threads to finish
    done

    # Advance time
    time_present=$time_forecast;

    calculate_ensemble_mean "prior" "Insta"
    calculate_ensemble_mean "prior" "Taver"

    # Estani's untested suggestion for parallel speedy launching:
    #seq 1 $members | xargs -n1 -P$NODE -Imember bash -x ensfcst.sh $SPEEDY ${ens_run_dir} ${time_present} $time_forecast member $N
}

#=======================================================================
#> @brief Estimates posterior ensemble using letkf executable
#=======================================================================
assimilation_step(){
    echo "============================================="
    echo "   Assimilation step"
    echo "============================================="

    cd $ens_run_dir
    #---------------------------------------
    # Link input files
    #---------------------------------------
    # - Guess fields
    for member in ${smembers[@]}; do
        ln -fs prior/${member}/${time_present}_grid_sigma_${A_mode}.rst gs01${member}.grd
    done

    # - Inflation factor
    if  [ -f infl_mul/${time_present}.grd ]; then
        ln -s infl_mul/${time_present}.grd infl_mul.grd
    fi

    #-------------------------------------------------------------
    # Run parallel LETKF program
    # ------------------------------------------------------------
    # To run system in background mpiexec must be redirected
    # from /dev/null (< /dev/null), otherwise process crashes:
    # "mpiexec_tux04 (handle_stdin_input 1089): stdin problem;
    #  if pgm is run in background, redirect from /dev/null
    #  e.g.: mpiexec -n 4 a.out < /dev/null & "

    mpiexec -n $CORES ./letkf.exe < /dev/null
    cat NOUT-000 # show observational departure
#    tail -n 18 NOUT-000 # show observational departure

    echo " - Archiving fields"

    # - Analysis members
    for member in ${smembers[@]}; do
        mv anal${member}.grd postr/${member}/${time_present}_grid_sigma_${A_mode}.rst
    done

    # - Inflation
    if [ -f infl_mul.grd ]; then
        cp infl_mul.grd infl_mul/${time_present}.grd
    fi

    ## - Prior Mean and spread
    #mv gues_me.grd prior/mean/${time_present}_grid_sigma_${A_mode}.rst
    #mv gues_sp.grd prior/spread/${time_present}_grid_sigma_${A_mode}.rst

    ## - Posterior Mean and spread
    #mv anal_me.grd postr/mean/${time_present}_grid_sigma_${A_mode}.rst
    #mv anal_sp.grd postr/spread/${time_present}_grid_sigma_${A_mode}.rst

    #  - Logs
    mv NOUT-000 log/${time_present}_rank-000.log
#    mv NOUT-001 ${ens_run_dir}/log/${time_present}_rank-001.log
#    mv NOUT-002 ${ens_run_dir}/log/${time_present}_rank-002.log
#    mv NOUT-003 ${ens_run_dir}/log/${time_present}_rank-003.log

    if [ "$A_mode" == "Taver" ]; then
        echo " - Recomposing instantaneous ensemble"
        for member in ${smembers[@]}; do
            cd ${ens_run_dir}/postr/${member}
            ln -fs ${ens_run_dir}/prior/${member}/${time_present}_grid_sigma_Tanom.rst .
            calculate_addition.exe ${time_present}_grid_sigma_Taver.rst ${time_present}_grid_sigma_Tanom.rst ${time_present}_grid_sigma_Insta.rst > /dev/null
        done

        calculate_ensemble_mean "postr" "Taver"
    fi

    calculate_ensemble_mean "postr" "Insta"

    echo "============================================="
}

save_metainfo(){
    cd $ens_run_dir

    echo $time_zero        >        time_zero.dat
    echo $time_start       >       time_start.dat
    echo $time_final       >       time_final.dat
    echo $time_step        >        time_step.dat
    echo $steps            >            steps.dat
    echo $run_name         >         run_name.dat
    echo $nature_name      >      nature_name.dat
    echo $station_set_name > station_set_name.dat
    echo $sampling_name    >    sampling_name.dat
    echo $A_mode           >           A_mode.dat
    echo $members          >          members.dat
    echo $save_members     >     save_members.dat
    touch dataset_complete.dat
}

#=======================================================================
#> @brief Removes temporary files
#=======================================================================
clean_dataset(){
    cd $ens_run_dir

    # Initial states, speedy programs and forcings
    for member in ${smembers[@]}; do
        rm $ens_run_dir/postr/$member/${time_zero}_grid_sigma_Insta.rst
        rm $ens_run_dir/postr/$member/at_gcm.exe
        rm $ens_run_dir/postr/$member/speedy_run.sh
        rm $ens_run_dir/postr/$member/fort.*
    done

    # ensemble members folders
    [ $save_members == "no" ] && rm -rf ${ens_run_dir}/prior/???
    [ $save_members == "no" ] && rm -rf ${ens_run_dir}/postr/???
    #[ $save_members == "no" ] && rm -rf ${ens_run_dir}/pos_F/???

    rm -f *.grd;  rm -f obs01.dat;
    rm -f $da_pgm
    rm -f NOUT*;  rm -f fort.21

    # For a free run postr and inflation fields are meaningless
    if [ "$station_set_name" == "station_none" ]; then
        rm -fr postr
        rm -fr infl_mul
        rm -fr obs
    fi
}

calculate_ensemble_mean(){
    [[ $# -eq 2 ]] || error "Usage: calculate_ensemble_mean ens_phase (prior|postr) state_kind (Insta|Taver)"
    ens_phase=$1 # prior or postr
    state_kind=$2 # Insta or Taver

    echo " Calculating $ens_phase $state_kind ensemble mean"

    cd ${ens_run_dir}/$ens_phase
    touch member_list.dat
    for member in ${smembers[@]}; do
        ln -fs ${member}/${time_present}_grid_sigma_${state_kind}.rst member${member}.rst
        echo "member${member}.rst" >> member_list.dat
    done

    mean.exe "member_list.dat" "ens_mean.rst"
    mv "ens_mean.rst" mean/${time_present}_grid_sigma_${state_kind}.rst
    rm member???.rst; rm member_list.dat
}


set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

# intermediate asignations just to fix doxygen calling graphs
da_pgm="letkf.exe"; dy1_pgm="at_gcm.exe"; dy2_pgm="speedy_run.sh"

ensemble_run $@
exit $?


#case $verbose in
    #0) msg_box=/dev/null;;
    #1) msg_box="out.txt; cat out.txt";;
    #*) error "Unsupported verbosity level"
#esac

#=========================================================================
#> @brief Estimates posterior ensemble using letkf executable
#=========================================================================
#analysis_Insta(){
    #cd $ens_run_dir
    ##---------------------------------------
    ## Link input files
    ##---------------------------------------
    ## - Guess fields
    #for member in $(seq $members)
    #do
        #member=$(printf '%03d' ${member})
        #ln -fs ${ens_run_dir}/prior/${member}/${time_present}_grid_sigma.rst gs01${member}.grd
    #done
    ## - Inflation factor
    #if  [ -f ${ens_run_dir}/infl_mul/${time_present}.grd ]; then
       #ln -s ${ens_run_dir}/infl_mul/${time_present}.grd infl_mul.grd
    #fi
    ##-------------------------------------------------------------
    ## Run parallel LETKF program
    ## ------------------------------------------------------------
    ## To run system in background mpiexec must be redirected
    ## from /dev/null (< /dev/null), otherwise process crashes:
    ## "mpiexec_tux04 (handle_stdin_input 1089): stdin problem;
    ##  if pgm is run in background, redirect from /dev/null
    ##  e.g.: mpiexec -n 4 a.out < /dev/null & "
    #mpiexec -n $CORES ./letkf.exe < /dev/null
##    cat NOUT-000 # show observational departure
##    tail -n 10 NOUT-000 # show observational departure
    ##-------------------------------------------------------------------
    ## Archive fields
    ##-------------------------------------------------------------------
    ## - Mean and spread
    #mv gues_me.grd ${ens_run_dir}/prior/mean/${time_present}_grid_sigma.rst
    #mv gues_sp.grd ${ens_run_dir}/prior/spread/${time_present}_grid_sigma.rst
    #mv anal_me.grd ${ens_run_dir}/postr/mean/${time_present}_grid_sigma.rst
    #mv anal_sp.grd ${ens_run_dir}/postr/spread/${time_present}_grid_sigma.rst
    ## - Analysis members
    #for member in $(seq $members)
    #do
            #member=$(printf '%03d' ${member})
            #mv anal${member}.grd ${ens_run_dir}/postr/${member}/${time_present}_grid_sigma.rst
    #done
    ## - Inflation
    #if [ -f infl_mul.grd ]; then
            #cp infl_mul.grd ${ens_run_dir}/infl_mul/${time_present}.grd
    #fi
    ## Save logs
    #mv NOUT-000 ${ens_run_dir}/log/${time_present}_rank-000.log
##    mv NOUT-001 ${ens_run_dir}/log/${time_present}_rank-001.log
##    mv NOUT-002 ${ens_run_dir}/log/${time_present}_rank-002.log
##    mv NOUT-003 ${ens_run_dir}/log/${time_present}_rank-003.log
#}

#=========================================================================
#> @brief Cleans ensemble members' output to keep the dataset small
#=========================================================================
#remove_members(){
    #for member in ${smembers[@]}; do
        #rm -f ${ens_run_dir}/prior/${member}/${time_present}*.grd
        #rm -f ${ens_run_dir}/postr/${member}/${time_present}*.grd
    #done
#}

#link_prior_to_postr(){
    #for member in ${smembers[@]}; do
        #ln -fs ${ens_run_dir}/prior/${member}/${time_present}_grid_sigma.rst ${ens_run_dir}/postr/${member}/${time_present}_grid_sigma.rst
    #done
#}
