#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
source $DAS_DIR/experiments.sh
source $DAS_DIR/test_list.sh
check_dependencies
declare -A span

#=======================================================================
# @brief Running script for L96_2s_mix model
#=======================================================================
model_script(){
    sim_id=$1

    par_sets[0]="link0.0"
    par_sets[1]="link0.1"
    par_sets[2]="link0.2"
    
    par_set=${par_sets[2]}
    ensemble_size=20
    addi_info="__m${ensemble_size}"
    exp_dir(){ echo "$STORE/$model/$par_set/${exp_name}${addi_info}";}
    #exp_dir(){ echo "$STORE/${exp_name}__infl_mode-cycle__m${ensemble_size}";}
    #line_detailed_stats="yes"
    line_detailed_stats="no"

    case $sim_id in
        "test") test_list ;;
        exp01) single_run "0.10" "0.01";;
        exp02) single_run "0.10" "0.10";;
#         exp03) surface_Taver_mode-tied_vs_infl__by_loc_radius ;;
#         exp04) surface_Taver_mode-tied_vs_infl__by_update_mode__obs_operator ;;
#         exp05) surface_Taver_mode-tied_vs_sat_level__by_update_mode__obs_operator ;;
#         exp06) surface_Taver_length_vs_cycle_length__by_update_mode__obs_operator ;;
#         exp07) line_Taver_mode-tied__by_update_mode__obs_operator ;;
        exp08) line_Taver_mode-tied__by_parameter "obs_operator" ;;
        exp09) line_Taver_mode-tied__by_parameter "obs_operator" "update_mode" ;;
        *    ) echo "No experiment with id $exp_id" 1>&2;;
    esac
    
}

model_script $@
