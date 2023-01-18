#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/experiments.sh
source $DAS_DIR/test_list.sh
check_dependencies
declare -A span

#=======================================================================
# @brief Running script for L96_2s_pf model
#=======================================================================
model_script(){
    sim_id=$1

    # c : Comp. 2 time scale factor
    # h : Coupling constant
    # b : Comp. 2 inverse amplitude factor

    par_sets[0]="c0.5_h0.5_b0.5"
    par_sets[1]="c0.5_h0.5_b1.0" # best one
    par_sets[2]="c0.5_h0.5_b4.0"
    par_sets[3]="c0.5_h1.0_b1.0"
    par_sets[4]="c0.1_h0.5_b0.5"
    par_sets[5]="c1.0_h0.3_b-1.0"
    par_sets[6]="c1.0_h0.3_b1.0"
    par_sets[7]="c0.5_h1.0_b1.0"

#     par_sets[8]="c0.5-h4.0-b4.0"    
#     par_sets[9]="c0.2-h4.0-b4.0"
#     par_sets[10]="c0.4-h4.0-b4.0"
#     par_sets[11]="c0.3_h4.0_b-4.0"
#     par_sets[12]="c0.4_h4.0_b-4.0"
#     par_sets[13]="c0.5_h4.0_b-4.0"
#     par_sets[14]="c0.5_h1.0_b-4.0"
#     par_sets[15]="c1.0_h1.0_b-4.0"
#     par_sets[16]="c1.0_h1.0_b4.0"
#     par_sets[17]="c1.0_h0.5_b4.0"

    par_set=${par_sets[7]}
    
    case $sim_id in
        test  ) test_list ;;
        runs01) single_run "0.10" "0.01"                 by "obs_operator";;
        runs02) single_run "0.30" "0.30"                 by "obs_operator";;
        surf00) surface "Taver_length" vs    "infl_enkf" by   "loc_radius";;
        surf01) surface "Taver_length" vs    "infl_enkf" by "obs_operator";;
        line01) line    "Taver_length"                   by "obs_operator";;
        surf02) surface "Taver_length" vs    "infl_enkf" by  "update_mode";;
        line02) line    "Taver_length"                   by  "update_mode";;
        surf03) surface "Taver_length" vs          "SNR" by "obs_operator";;
        surf04) surface "Taver_length" vs "cycle_length" by "obs_operator";;
        surf05) surface "Taver_length" vs       "F_ampl" by "obs_operator";;
        line10) verity=1; line    "Taver_length"        by "obs_operator";;
        line11) verity=1; line    "Taver_length"        by "update_mode";;
        surf13) surface  "range_upper" vs  "range_lower" by "obs_operator";;
        *    ) echo "No experiment with id $exp_id" 1>&2;;
    esac
}

model_script $@
