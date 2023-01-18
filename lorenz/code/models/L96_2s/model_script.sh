#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/experiments.sh
source $DAS_DIR/test_list.sh
check_dependencies
declare -A span

#=======================================================================
# @brief Running script for L96_2s model
#=======================================================================
model_script(){
    sim_id=$1
    # c : Comp. 2 time scale factor
    # h : Coupling constant
    # b : Comp. 2 inverse amplitude factor
    par_sets[0]="c0.1_h1.0-b1.0"
    par_sets[1]="c0.1_h1.0-b0.5"
    par_sets[2]="c0.1_h1.0-b4.0"
    par_sets[3]="c0.1_h0.5-b1.0"
    par_sets[4]="c0.1_h2.0-b1.0"
    par_sets[5]="c0.1_h2.0-b4.0"
    par_sets[6]="c0.1_h4.0-b1.0"
    par_sets[7]="c0.1_h4.0-b4.0"

    par_sets[10]="c0.2-h4.0-b4.0"
    par_sets[11]="c0.2-h1.0-b1.0"

    par_sets[20]="c0.3_h4.0_b-4.0"
    par_sets[21]="c0.4_h4.0_b-4.0"
    par_sets[22]="c0.4-h4.0-b4.0"

    par_sets[30]="c0.5_h4.0_b-4.0"
    par_sets[31]="c0.5-h4.0-b4.0"

    par_sets[41]="c1.0_h0.3_b1.0"
    par_sets[42]="c0.5_h1.0_b1.0" # ClimDyn paper configuration  
    
    par_set=${par_sets[42]}

    case $sim_id in
        "test") test_list ;;
	runs01)
	    resp_window_size=wide;   n_obs=40;
	    single_run "0.10" "0.01"                 by "obs_operator";;
	runs02)
	    resp_window_size=narrow; n_obs=40;
	    single_run "0.10" "0.01"                 by "obs_operator";;
        # runs02) single_run "0.30" "0.30"                 by "obs_operator";;
        # surf01) surface "Taver_length" vs    "infl_enkf" by  "update_mode";;
	line01)
	    resp_window_size=wide;   n_obs=40;
	    line    "Taver_length"                   by  "obs_operator";;
	line02)
	    resp_window_size=narrow; n_obs=40;
	    line    "Taver_length"                   by  "obs_operator";;
	surf05)
	    resp_window_size=wide;   n_obs=40;
	    surface "Taver_length" vs "SNR"          by  "obs_operator";;
        surf03)
	    n_obs=40;
	    surface  "range_upper" vs  "range_lower" by "obs_operator";;
        # surf02) surface "Taver_length" vs    "infl_enkf" by "obs_operator";;
        # line02) line    "Taver_length"                   by "obs_operator";;
        # surf04) surface            "c" vs            "b" by "obs_operator";;
        # line10)  verity=1; line    "Taver_length"        by "obs_operator";;
        # line11)  verity=1; line    "Taver_length"        by "update_mode";;
  
        *     ) echo "No experiment with id $sim_id" 1>&2;;
    esac
}

model_script $@
