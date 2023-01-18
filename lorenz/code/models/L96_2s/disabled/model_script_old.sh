#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ${COM_DAS_DIR}/common_das_tools.sh
check_dependencies
#===================================================================
# L96_2s model simulation list
#===================================================================
[[ $# -eq 1 ]] || error "Incorrect argument #"
simulation_id=$1

source $CODE_DIR/$model/default_config.sh

case $simulation_id in
    #===============================================================
    #> @page sim_list
    #> 2013_11_14: Filter tuning for L96_2s
    #===============================================================
    "2013_11_14")
        filter_tuning(){
            run_free_set_dir="$STORE/$model/run_free_sets/link${link}_cycle_length"
            run_assi_set_pair_dir="$STORE/$model/filter_tunning"
            run_assi_set_pair.sh -r "$run_free_set_dir" "$run_assi_set_pair_dir" \
                "xlocal" "linspace(1,8,8)" ; }
        ( export link="0.1d0"; filter_tuning )
        ;;
    #===============================================================
    #> @page sim_list
    #> 2013_11_30: Run free sets for L96_2s model
    #> 2013_11_30: update_mode_vs_obs_operator bunch of
    #>             SNR_vs_cycle_length run_sets
    #===============================================================
    "2013_11_30")
        cl_span="arange(1,102,5)*$dt"
        run_free_set_dir="$STORE/run_free_sets/cycle_length_${cl_span}"
        run_set.sh --run_mode=free --par1=cycle_length --span1="$cl_span" \
          $run_free_set_dir || error

        run_assi_set_bunch_dir="$STORE/run_assi_sets/update_mode_vs_obs_operator/SNR_vs_cycle_length"
        run_set_bunch.sh --ref_dir="$run_free_set_dir" \
          --bunch_par1=update_mode  --bunch_span1="['Hakim','Augm1']" \
          --bunch_par2=obs_operator --bunch_span2="['plus','vsl0','vsl1']" \
          --set_par1=SNR            --set_span1="linspace(0.5,10,8)" \
          "$run_assi_set_bunch_dir" || error
        ;;  
    ##===============================================================
    ##> @page sim_list
    ##> 2013_11_21: cycle_length_vs_noise_study
    ## vsl1_augm1 crashed
    ##===============================================================
    #"2013_11_26")
        #run_free_set_dir="$STORE/$model/run_free_sets/cycle_length_arange(1,102,5)dt"
##            run_set.sh -m free $run_free_set_dir cycle_length "arange(1,102,5)*$dt"

        #cycle_length_vs_noise_study(){
            #run_assi_set_pair_dir="$STORE/$model/cycle_length_vs_noise_study/obsOp_${obs_operator}"
            #run_assi_set_pair.sh -r "$run_free_set_dir" "$run_assi_set_pair_dir" "SNR" "linspace(0.5,10,8)"
        #}
        #( export obs_operator="plus"; cycle_length_vs_noise_study )
        #( export obs_operator="vsl0"; cycle_length_vs_noise_study )
        #( export obs_operator="vsl1"; cycle_length_vs_noise_study )
        #;;
    *)  echo " No simulation with id $simulation_id" ;;
esac


