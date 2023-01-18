#!/usr/bin/env bash
#=======================================================================
#> @brief Runs free_ensemble_run for different already existing nature
#         runs using appropiate free ensemble names
#=======================================================================
das_plot(){
#	plotting_list "equal_time_lenght_1"
#	plotting_list "equal_time_lenght_2"
#	plotting_list "equal_time_lenght_3"
#	plotting_list "equal_time_lenght_4"
#	plotting_list "equal_assim_steps"
	plotting_list "Homo2_equal_assim_steps"
}

plotting_list(){
    [[ $# -eq 1 ]] || error "Usage: plotting_list plot_set_id"
    plot_set_id=$1

    case $plot_set_id in
    "equal_time_lenght_1")
		plot_nature_run.sh "nature_0024hr_0256ts_spinup1"
		plot_ensemble_run.sh "free-run_0024hr_0256ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0024hr_0256ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "equal_time_lenght_2")
		plot_nature_run.sh "nature_0048hr_0128ts_spinup1"
		plot_ensemble_run.sh "free-run_0048hr_0128ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0048hr_0128ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "equal_time_lenght_3")
		plot_nature_run.sh "nature_0096hr_0064ts_spinup1"
		plot_ensemble_run.sh "free-run_0096hr_0064ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0096hr_0064ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "equal_time_lenght_4")
		plot_nature_run.sh "nature_0192hr_0032ts_spinup1"
		plot_ensemble_run.sh "free-run_0192hr_0032ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0192hr_0032ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "Homo2_equal_assim_steps")
		plot_nature_run.sh "nature_0024hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0024hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0024hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		plot_nature_run.sh "nature_0048hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0048hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0048hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		plot_nature_run.sh "nature_0096hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0096hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0096hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		plot_nature_run.sh "nature_0192hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0192hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0192hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		plot_nature_run.sh "nature_0384hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0384hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0384hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		plot_nature_run.sh "nature_0768hr_0100ts_spinup1"
		plot_ensemble_run.sh "free-run_0768hr_0100ts_m20_samp3"
		plot_ensemble_run.sh "assim-run_0768hr_0100ts_m20_samp3_sta-Homo2_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "TreeMax_equal_assim_steps")
		plot_ensemble_run.sh "assim-run_0024hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		plot_ensemble_run.sh "assim-run_0048hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		plot_ensemble_run.sh "assim-run_0096hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		plot_ensemble_run.sh "assim-run_0192hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		plot_ensemble_run.sh "assim-run_0384hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		plot_ensemble_run.sh "assim-run_0768hr_0100ts_m20_samp3_sta-TreeMax_obs-STemp_SNR-2.0_Taver-DA"
		;;
    "the_rest")
    
	    #pattern="nature*"
	    #find . -maxdepth 1 -type d -name "${pattern}" | sort | cut -c3- > list.tmp
	    #cat list.tmp | while read nature_run_name; do 
			#echo "Plotting $nature_run_name"
			#plot_nature_run.sh $nature_run_name
	    #done
	
	
	    pattern="free-run*"
	    pattern="assim-run*"
	    find . -maxdepth 1 -type d -name "${pattern}" | sort | cut -c3- > list.tmp
	    cat list.tmp | while read ens_run_name; do 
			echo "Plotting $ens_run_name"
			plot_ensemble_run.sh $ens_run_name
	    done
	    ;;
	*)
		echo " No plot set with id $plot_set_id"
		;;
    esac

    rm -f list.tmp
    return 0 # Success
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

das_plot $@
exit $?
