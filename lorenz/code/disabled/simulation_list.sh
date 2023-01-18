#!/usr/bin/env bash
#===================================================================
#> @page sim_list  L96_2s_mix model simulation batch 1
#===================================================================
simulation_list(){
    [[ $# -eq 1 ]] || error "Incorrect argument #"
    simulation_id=$1 # Simulation identifier

    echo " Launching simulation $simulation_id"

    model="L96_2s_mix"; das_make_all.sh $model
    source $CODE_DIR/$model/default_config.sh

    case $simulation_id in
        #===============================================================
        #> @page sim_list
        #> 2013_11_13: Run free sets for L96_2s_mix model
        #===============================================================
        "2013_11_13")
            export link="0.1d0"
            run_free_set_dir="$STORE/$model/run_free_sets/link_${link}/cycle_length_arange(1,102,5)dt"
            run_set.sh -m free $run_free_set_dir cycle_length "arange(1,102,5)*$dt"

            run_free_set_dir="$STORE/$model/run_free_sets/link_linspace(0,0.2,8)/cycle_length_arange(1,102,5)dt"
            run_set.sh -m free $run_free_set_dir link "linspace(0,0.2,8)" cycle_length "arange(1,102,5)*$dt"

            #( export link="0.1d0"
                #run_free_set_dir="$STORE/$model/run_free_sets/link${link}_cycle_length"
                #run_set.sh -m free $run_free_set_dir cycle_length "arange(1,42,5)*$dt" )
                
            #run_free_set_dir="$STORE/$model/run_free_sets/link_&_cycle_length"
            #run_set.sh -m free $run_free_set_dir \
                #link "linspace(0,0.2,8)" cycle_length "arange(1,42,5)*$dt"
            ;;
        #===============================================================
        #> @page sim_list
        #> 2013_11_14: Filter tuning for L96_2s_mix model
        #>   - Best xlocal value for Hakim update_mode and obs every 2
        #>     gridpoints was exactly 2
        #>   - Augm1 assi run crashed!!!! resend it
        #===============================================================
        "2013_11_14")
            filter_tuning(){
                run_free_set_dir="$STORE/$model/run_free_sets/link${link}_cycle_length"
                run_assi_set_pair_dir="$STORE/$model/filter_tunning"
                run_assi_set_pair.sh -r "$run_free_set_dir" "$run_assi_set_pair_dir" \
                    "xlocal" "linspace(1,8,8)" ; }
            ( export link="0.1d0"; filter_tuning )
        # Unsuccesful attempt to track down Augm1 tunning bug    
        #"2013_11_20")
            #run_free_dir=$test_dir/run_free
            #run.sh -m free "$run_free_dir"; das_stats.sh "$run_free_dir"

            #run_assi_dir(){ echo $test_dir/run_assi_$update_mode; }
            #(   export update_mode='Augm1'
                #run.sh -m assi -r "$run_free_dir" "$(run_assi_dir)" )
            ;;
        #=============================================================
        #>@page sim_list
        #>2013_11_20: Coupling influence for L96_2s_mix model
        #==============================================================
        "2013_11_20")
            run_free_set_dir="$STORE/$model/run_free_sets/link_linspace(0,0.2,8)/cycle_length_arange(1,102,5)dt"
            run_set.sh -m free $run_free_set_dir link "linspace(0,0.2,8)" cycle_length "arange(1,102,5)*$dt"
            cycle_length_vs_coupling_study(){
                run_assi_set_pair_dir="$STORE/$model/cycle_length_vs_coupling_study/SNR_${SNR}/obsOp_${obs_operator}"
                run_assi_set_pair.sh -r "$run_free_set_dir" "$run_assi_set_pair_dir"
            }
            export SNR="2.0"
            ( export obs_operator="plus"; cycle_length_vs_coupling_study )
            ( export obs_operator="vsl0"; cycle_length_vs_coupling_study )
            ( export obs_operator="vsl1"; cycle_length_vs_coupling_study )
            ;;
        #===============================================================
        #> @page sim_list
       #> 2013_11_21: cycle_length_vs_noise_study
        #===============================================================
        "2013_11_21_")
            export link="0.1d0"
            run_free_set_dir="$STORE/$model/run_free_sets/link_${link}/cycle_length_arange(1,102,5)dt"
#            run_set.sh -m free $run_free_set_dir cycle_length "arange(1,102,5)*$dt"

            cycle_length_vs_noise_study(){
                run_assi_set_pair_dir="$STORE/$model/cycle_length_vs_noise_study/link_${link}/obsOp_${obs_operator}"
                run_assi_set_pair.sh -r "$run_free_set_dir" "$run_assi_set_pair_dir" "SNR" "linspace(0.5,10,8)"
            }
            #( export obs_operator="plus"; cycle_length_vs_noise_study )
            #( export obs_operator="vsl0"; cycle_length_vs_noise_study )
            ( export obs_operator="vsl1"; cycle_length_vs_noise_study )
            ;;
        *)  echo " No simulation with id $simulation_id"
            ;;
    esac
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source $(pwd)/das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh

simulation_list $@
exit $?
