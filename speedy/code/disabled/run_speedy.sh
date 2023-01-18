#!/bin/bash
#=======================================================================
#> @brief Wrapper script to run SPEEDY model in folder $input_dir
#> Restart and forcing files must be linked or copied there
#=======================================================================
run_speedy()
{
	# Reading external arguments
	istart=$1 # Input  restart mode (info in com_tsteps.h)
	jstart=$2 # Output restart mode (info in com_tsteps.h)
	present_time=$3
	forecast_time=$4
	input_dir=$5
	# forcings_dir=$6
			
	# Parsing external arguments
	if [ $# -gt 5 ]; then
		echo "ta_run_speedy: Incorrect number of arguments"
	exit 1
	fi

	cd $input_dir

	# Setting restart mode and initial time
	echo $istart                      > fort.2
	echo $jstart                     >> fort.2

	# Set initial time
	echo  $present_time | cut -c1-4   > fort.7 # year
	echo  $present_time | cut -c5-6  >> fort.7 # month
	echo  $present_time | cut -c7-8  >> fort.7 # day
	echo  $present_time | cut -c9-10 >> fort.7 # hour

	# Set final time    
	echo $forecast_time | cut -c1-4   > fort.8 # year
	echo $forecast_time | cut -c5-6  >> fort.8 # month
	echo $forecast_time | cut -c7-8  >> fort.8 # day
	echo $forecast_time | cut -c9-10 >> fort.8 # hour

	# Set initial conditions (restart files)
	if   [ $istart == 0 ]; then # Reference atmosphere
		 : 
	elif [ $istart == 2 ]; then # Instantaneous 
		 ln -fs ${input_dir}/${present_time}.grd fort.90
	elif [ $istart == 3 ]; then # Time average decomposed
		 ln -fs ${input_dir}/${present_time}_TA.grd fort.92
		 ln -fs ${input_dir}/${present_time}_AN.grd fort.93
	else
		 echo "run_speedy error: invalid istart value $istart"
		 exit 1
	fi

	# Running speedy
	./imp.exe > speedy.log

	# patch for filenames conflict
	mv state_ta_average.grd ${forecast_time}_TA.grd
	mv state_ta_anomaly.grd ${forecast_time}_AN.grd

	# Checking calendar consistency
	forecast_time_ATGCM=$(cat fort.87)
	if [ $forecast_time_ATGCM -ne $forecast_time ]; then
		echo "Calendar system conflict between bash and fortran components!!"
		exit 1
	fi

	if [ $# -gt 6 ]; then
		output_dir=$6
		mv ${forecast_time}*.grd ${output_dir}
		
	# Storing final state
	#echo $input_dir
	#echo $output_dir
	#if [ "$input_dir" -ne "$output_dir" ]; then
	#  if   [ $jstart == 1 ]; then
	#     mv state_ta_average.grd ${output_dir}/${forecast_time}.grd
	#     mv state_ta_anomaly.grd ${output_dir}/${forecast_time}_p.grd
	#  elif [ $jstart == 2 ]; then
	#     mv ${forecast_time}.grd ${output_dir}/${forecast_time}.grd
	#     mv ${forecast_time}_p.grd ${output_dir}/${forecast_time}_p.grd
	#     mv state_ta_average.grd ${output_dir}/${forecast_time}_TA.grd
	#     mv state_ta_anomaly.grd ${output_dir}/${forecast_time}_AN.grd
	#  else
	#     echo "run_speedy error: invalid jstart value $jstart"
	#     exit 1
	#  fi
	fi

	# Storing Spectraly filtered analysis fields (currently disabled) 
	#mv ${present_time}_TA.grd ${archive_dir}/anal_f/${member}
	#mv ${present_time}_AN.grd ${archive_dir}/anal_f/${member}

	# Storing descriptor files (currently disabled) 
	#mv ${PRESENT_TIME}_TA.ctl ${NATURE_DIR}/yyyymmddhh_TA.ctl
	#    mv ${forecast_time}_TA.ctl ${NATURE_DIR}/yyyymmddhh_TA.ctl
	#    mv ${forecast_time}_AN.ctl ${NATURE_DIR}/yyyymmddhh_AN.ctl

	return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

run_speedy $1 $2 $3 $4 $5
exit $?
