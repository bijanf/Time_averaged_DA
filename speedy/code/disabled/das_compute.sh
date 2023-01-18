#!/usr/bin/env bash
#=======================================================================
#> @brief Main calculation launcher
#=======================================================================
das_compute(){
    . das_initialize.sh

    sim_id="2013_09_02"
    echo "==============================================="
    echo " Running simulation called ${sim_id}"
    echo "==============================================="
    echo " Check log/${sim_id}.log to see the progress"  
  
    { time -p simulation_list $sim_id; } &> "$DAS/log/${sim_id}.log" &
}

simulation_list(){
    [[ $# -eq 1 ]] || error "Usage: simulation_list simulation_id"
    simulation_id=$1
    echo " Launching simulation $simulation_id"
    #=====================================================================
    #> @page production "Exploration log"
    #> @brief List of results
    #=====================================================================
   
    case $simulation_id in
    "prehistory");;
        #=====================================================================
        #> @page production
	#> - prehistory:
	#> Quick and dirty 3 month long runs with default filter configuration,
	#> RMSE seems to have a positive trend specially rain, temperature and humidity.
        #=====================================================================
    "2013_08_28")
        #=====================================================================
        #> @page production
        #> - 2013_08_28: Preliminar set of runs with equal duration and different TA periods.
        #=====================================================================
	rm report.tmp; touch report.tmp
	
	echo "spinup"
	time_length_array=(1 2) # In years
	create_spinups 1979010100
    
	echo "sampling" # (No name generation so far)
	sampling.sh spinup_1979010100-start_002-yr 168 100 sampling_100steps_168hours | tee $ARCHIVE_DIR/sampling_100steps_168hours.log
    
	echo "nature_run"
	launch_nature_run spinup_1979010100-start_001-yr $((2**8)) $((24*1))  > report.tmp &
	launch_nature_run spinup_1979010100-start_001-yr $((2**7)) $((24*2))  > report.tmp &
	launch_nature_run spinup_1979010100-start_001-yr $((2**6)) $((24*4))  > report.tmp &
	launch_nature_run spinup_1979010100-start_001-yr $((2**5)) $((24*8))  > report.tmp &
	##                 spinup_name                      steps   time_step
	## 587 secs
	wait
    
	echo "free_ensemble_run"
	create_free_ensemble_runs sampling_100steps_168hours 20 no "nature*" >> report.tmp
	# 3778 + 3128 + 2829+ 2680 secs =12415 sec ~ 3.44 hr 

	echo "assim_ensemble_run"
	create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Taver" "free-run*" >> report.tmp
	#                         |    station_set_name  | obs config | SNR | A_mode |  free run pattern
	# 4654.92 + 3509.89 + 3006.21 + 2777.77 = 13946 sec = 3.87 hr
	;;
    "2013_08_29")
	echo "the long one"
	launch_nature_run spinup_1979010100-start_001-yr $((2**4)) $((24*16))
        #                 spinup_name                      steps   time_step
 	create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0384hr_0016ts_spinup1"
	# to be run
	create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Taver" "free-run_0384hr_0016ts_m20_samp2"
	#                         |    station_set_name      | obs    | SNR | A_mode |  free run pattern
        ;;
    "2013_08_30")
        #=====================================================================
        #> @page production
        #> - 2013_08_30: set of nature runs with equal number of time leaps
        #=====================================================================
	echo "nature_run"
	time_step_array=(24 48 96 192 384 768)
	create_nature_runs "spinup_1979010100-start_001-yr" 100
	;;
    "2013_08_31_1")
        #=====================================================================
        #> @page production
        #> - 2013_08_31_1: set of free and assim runs corresponding to 2013_08_30
	#> nature runs (with equal assim step number to calculate skill right)
        #=====================================================================
#	echo "nature_run"
	leaps=100; s_leaps=$(printf '%04d' $leaps)
	echo "free_ensemble_run"
	create_free_ensemble_runs sampling_100steps_168hours 20 no "nature*${s_leaps}ts*"
	# run with  768hr leap was aborted. too long.
	;;
    "2013_08_31_2")
        #=====================================================================
        #> @page production
        #> - 2013_08_31_2: set of free and assim runs corresponding to 2013_08_30
	#> nature runs (with equal assim step number to calculate skill right)
        #=====================================================================
	leaps=100; s_leaps=$(printf '%04d' $leaps)
	echo "assim_ensemble_run"
	create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Taver" "free-run*${s_leaps}ts*"
	;;
    "2013_09_01")
        #=====================================================================
        #> @page production
        #> - 2013_08_31_2: set of free and assim runs corresponding to 2013_08_30
	#> nature runs (with equal assim step number to calculate skill right)
        #=====================================================================
	leaps=100; s_leaps=$(printf '%04d' $leaps)
	echo "assim_ensemble_run"
	create_assim_ensemble_runs "station_homogeneous_gap2" "Origi" "2.0" "Taver" "free-run*${s_leaps}ts*"
	# assim-run_0096hr_0100ts_m20_samp3_sta-Homo2_obs-Origi_Taver-DA crashed due to 
	# *** Model variables out of accepted range *** error in speedy
	;;
    "2013_09_01_2")
        #=====================================================================
        #> @page production
        #> - 2013_08_31_2: set of free and assim runs corresponding to 2013_08_30
	#> nature runs (with equal assim step number to calculate skill right)
        #=====================================================================
	echo "very long runs"
	echo "free_ensemble_run"
	create_free_ensemble_runs sampling_100steps_168hours 20 no "nature*0768hr*"
	echo "assim_ensemble_run"
	create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Taver" "free-run*0768hr*"
	;;
    "2013_09_02")
        #=====================================================================
        #> @page production
        #> - 2013_08_31_2: set of free and assim runs corresponding to 2013_08_30
	#> nature runs (with equal assim step number to calculate skill right)
        #=====================================================================
	echo "assim_ensemble_run"
	create_assim_ensemble_runs "station_tree_coverage_max" "STemp" "2.0" "Taver" "free-run*0100ts*"
	;;

    *)
	echo " No simulation with id $simulation_id"
	;;
    esac
    #-------------------------------------------------------------------
    # spinup
    #-------------------------------------------------------------------
    #time_length_array=(1 2) # In years
    #create_spinups 1979010100

    #-------------------------------------------------------------------
    # sampling (No name generation so far)
    #-------------------------------------------------------------------
    #sampling.sh spinup_1979010100-start_002-yr 6 100 sampling_100steps_6hours | tee $ARCHIVE_DIR/sampling_100steps_6hours.log
    #sampling.sh spinup_1979010100-start_002-yr 12 100 sampling_100steps_12hours | tee $ARCHIVE_DIR/sampling_100steps_12hours.log
    
    #sampling.sh spinup_1979010100-start_002-yr 168 100 sampling_100steps_168hours | tee $ARCHIVE_DIR/sampling_100steps_168hours.log

    ##-------------------------------------------------------------------
    #echo "nature_run"
    ##-------------------------------------------------------------------
    ##time_step_array=(6 12 24 48 96 192 384)
    ##time_step_array=(24 48 96 192)
    ##                 spinup_name                      steps   time_step
    #launch_nature_run spinup_1979010100-start_001-yr $((2**4)) $((24*16))  > report.tmp &
    #wait

#	nature_run.sh  24 200 test_nature
    #create_nature_runs "spinup_1979010100-start_001-yr" 2
    #create_nature_runs spinup_1979_1year 300
    #create_nature_runs spinup_name       steps

    #rm report.tmp; touch report.tmp
    #-------------------------------------------------------------------
    #echo "free_ensemble_run"
    #-------------------------------------------------------------------
    #create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0006hr_0002ts_spin1"
    #create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0006hr_0300ts_spin1"
    #create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0012hr_0300ts_spin1"
    #create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0024hr_0300ts_spin1"
    #create_free_ensemble_runs sampling_100steps_168hours 20 no "nature_0048hr_0300ts_spin1"
    
    #-------------------------------------------------------------------
    #echo "assim_ensemble_run"
    #-------------------------------------------------------------------
    #create_assim_ensemble_runs "station_homogeneous_gap2"  "Origi" "2.0" "Insta" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_homogeneous_gap2"  "Origi" "2.0" "Taver" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_tree_coverage_max" "Origi" "2.0" "Insta" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_tree_coverage_max" "Origi" "2.0" "Taver" "free-run_*_0002ts_m20_samp2"
    #-------------------------------------------------------------------
    #create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Insta" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_homogeneous_gap2"  "STemp" "2.0" "Taver" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_tree_coverage_max" "STemp" "2.0" "Insta" "free-run_*_0002ts_m20_samp2"
    #create_assim_ensemble_runs "station_tree_coverage_max" "STemp" "2.0" "Taver" "free-run_*_0002ts_m20_samp2"
    #--------------------------|--------------------------|--------|-----|--------|-------------------------------
    #                          |    station_set_name      | obs    | SNR | A_mode |  free run pattern
    #                                                       config
}

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
        time_length_hr=$(( 8760 * time_length ))

        echo " Generating name"
        s_time_length=$(printf '%03d' $time_length)
        spinup_name="spinup_${time_start}-start_${s_time_length}-yr"
        echo $spinup_name >> spinups.dat

        echo " Creating $spinup_name"
	{ time -p spinup.sh $time_start $time_length_hr $spinup_name $ARCHIVE_DIR; } &> ${spinup_name}.log &
		#time -p spinup.sh $time_start $time_length_hr $spinup_name $ARCHIVE_DIR > ${spinup_name}.log &
    done
	wait

    echo "Storing logs"
    cat spinups.dat | while read spinup_name; do 
        mv ${spinup_name}.log ${spinup_name}/${spinup_name}.log
	echo "spinup" > ${spinup_name}/dataset_type.dat
    done
    
    rm -f spinups.dat
    echo "==============================================="
    echo " spinup RUNS GENERATED SUCCESSFULLY"
    echo "==============================================="

    return 0 # Success
}

#=======================================================================
#> @brief Runs create_nature_run for different time_steps using 
#>        appropiate nature names
#=======================================================================
create_nature_runs(){
    [[ $# -eq 2 ]] || error "Usage: create_nature_runs spinup_name steps"
        spinup_name=$1
              steps=$2
#    time_step_array=$3

    echo "==========================================================="
    echo " GENERATING A SET OF NATURE RUNS WITH DIFFERENT TIME STEPS"
    echo "==========================================================="

    cd $ARCHIVE_DIR
    rm -f touch nature_runs.dat; touch nature_runs.dat
    for time_step in ${time_step_array[@]}
    do
        # Nature name generation
            s_steps=$(printf '%04d' $steps)
        s_time_step=$(printf '%04d' $time_step)
        case "$spinup_name" in
             "spinup_1979010100-start_001-yr") s_spinup="spinup1";;
                                            *) error "Unknown spinup_name $spinup_name";;
        esac            
        nature_name="nature_${s_time_step}hr_${s_steps}ts_${s_spinup}"
        echo $nature_name >> nature_runs.dat
        echo " Creating $nature_name"
        { time -p nature_run.sh $spinup_name $time_step $steps $nature_name; } &> ${nature_name}.log &

    done

    wait

    # Store logs
    cat nature_runs.dat | while read nature_name; do 
        mv ${nature_name}.log ${nature_name}/${nature_name}.log
    done
    
    rm -f nature_runs.dat
    echo "==========================================================="
    echo " NATURE RUNS GENERATED SUCCESSFULLY"
    echo "==========================================================="

    return 0 # Success
}

#=======================================================================
#> @brief Runs nature_run with the appropiate nature name
#=======================================================================
launch_nature_run(){
    [[ $# -eq 3 ]] || error "Usage: launch_nature_run spinup_name steps time_step"
    spinup_name=$1
          steps=$2
      time_step=$3

    cd $ARCHIVE_DIR

	# Nature name generation
		s_steps=$(printf '%04d' $steps)
	s_time_step=$(printf '%04d' $time_step)
	case "$spinup_name" in
		 "spinup_1979010100-start_001-yr") s_spinup="spinup1";;
										*) error "Unknown spinup_name $spinup_name";;
	esac            
	nature_name="nature_${s_time_step}hr_${s_steps}ts_${s_spinup}"

	echo " Creating $nature_name"
	{ time -p nature_run.sh $spinup_name $time_step $steps $nature_name; } &> ${nature_name}.log

    # Store log
	mv ${nature_name}.log ${nature_name}/${nature_name}.log

    return 0 # Success
}


#nature_name(){
	#echo "nature_${s_time_step}hr_${s_steps}ts_${s_spinup}"
#}

#=======================================================================
#> @brief Runs free_ensemble_run for different already existing nature
#         runs using appropiate free ensemble names
#=======================================================================
create_free_ensemble_runs(){
    [[ $# -eq 4 ]] || error "Usage: create_free_ensemble_runs sampling_name members save_members pattern"
    sampling_name=$1
          members=$2
     save_members=$3
          pattern=$4
          
    echo "==========================================================="
    echo " GENERATING A SET OF FREE RUNS GIVEN A LIST OF FREE RUNS"
    echo "==========================================================="

    cd $ARCHIVE_DIR
    nature_run_list="nature_runs.dat"
    find . -maxdepth 1 -type d -name "${pattern}" | sort | cut -c3- > $nature_run_list

    cat $nature_run_list | while read nature_run_name; do 

        # Setting free_run_name
        case "$sampling_name" in
        "sampling_100steps_12hours" ) s_sampling="samp2";;
	"sampling_100steps_168hours") s_sampling="samp3";;
                                  *) error "Unknown sampling_name $sampling_name";;
        esac
                    
        free_run_name="free-run_${nature_run_name:7:13}_m${members}_${s_sampling}"

        # Launching create_free_ensemble_run.sh
        echo " - Creating ${free_run_name}"
        { time -p free_ensemble_run.sh $free_run_name $nature_run_name $sampling_name $members $save_members; } &> ${free_run_name}.log
        mv ${free_run_name}.log ${free_run_name}/${free_run_name}.log
	

    done

    rm -f $nature_run_list
    
    echo "==========================================================="
    echo " FREE RUNS GENERATED SUCCESSFULLY"
    echo "==========================================================="

    return 0 # Success
}

#=======================================================================
#> @brief Runs create_nature_run for different time_steps using 
#>        appropiate nature names
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
        { time -p assim_ensemble_run.sh $assim_run_name $free_run_name $station_set_name $obs_config $A_mode; } &> ${assim_run_name}.log
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

das_compute $@
exit $?
