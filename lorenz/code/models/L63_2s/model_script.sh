#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/experiments.sh
source $DAS_DIR/test_list.sh
check_dependencies
declare -A span

#=======================================================================
# @brief Running script for L63_2s model
#=======================================================================
model_script(){
    sim_id=$1

    par_sets[0]="uncoupled"
    par_sets[1]="weather_convection"
    par_sets[2]="extratropical_Ocen_atm_strong_coupling"
    par_sets[3]="extratropical_Ocen_atm_weak_coupling"
    par_sets[4]="enso"
    par_sets[5]="shallow-deep_soil_weak_coupling"
    par_sets[6]="shallow-deep_soil_mid_coupling"
    par_sets[7]="shallow-deep_soil_strong_coupling"
    par_sets[8]="shallow-deep_soil_weak_coupling_minus"

    par_sets[10]="enso-c0.00"
    par_sets[11]="enso-c0.10"
    par_sets[12]="enso-c0.20"
    par_sets[13]="enso-c0.22"
    par_sets[14]="enso-c0.23"
    par_sets[15]="enso-c0.24"
    par_sets[16]="enso-c0.25"
    par_sets[17]="enso-c0.30"
    par_sets[18]="enso-c0.40"
    par_sets[19]="enso-c0.50"
    par_sets[20]="enso-c0.55"
    par_sets[21]="enso-c0.60"
    par_sets[22]="enso-c0.70"
    par_sets[23]="enso-c0.90"
    par_sets[24]="enso-c1.00"

    export par_set=${par_sets[11]}
    
    Taver_span_sizes=("small" "mid" "big")
    export Taver_span_size=${Taver_span_sizes[2]}

    case $sim_id in
        test  ) test_list ;;
        runs01) single_run "0.10" "0.01"                 by "obs_operator";;
        runs02) single_run "0.30" "0.30"                 by "obs_operator";;
        runs03) single_run "0.30" "0.30"                 by  "update_mode";;
        surf01) surface "Taver_length" vs    "infl_enkf" by  "update_mode";;
        line01) line    "Taver_length"                   by  "update_mode";;
        surf02) surface          "eta" vs    "infl_enkf" by  "update_mode";;
        surf13) surface "Taver_length" vs    "infl_enkf" by  "update_mode" "comp_localization";;
        line13) line    "Taver_length"                   by  "update_mode" "comp_localization";;
                #surf02) surface "Taver_length" vs    "infl_enkf" by "obs_operator";;
        #line02) line    "Taver_length"                   by "obs_operator";;
        # surf03) surface "Taver_length" vs    "infl_enkf" by "update_mode" "obs_operator";;
        # line03) line    "Taver_length"                   by "update_mode" "obs_operator";;
        surf03) surface "Taver_length" vs    "infl_enkf" by           "dt" ;;
        surf04) surface "Taver_length" vs    "infl_enkf" by  "update_mode" "eta";;
        line04) line    "Taver_length"                   by  "update_mode" "eta";;
        surf05) surface "Taver_length" vs          "SNR" by "obs_operator" ;;
        surf06) surface "Taver_length" vs "cycle_length" by "obs_operator" ;;
        surf07) surface            "S" vs           "k1" by "obs_operator" ;;
        surf08) surface            "S" vs          "tao" by "obs_operator" ;;
        surf09) surface "Taver_length" vs            "S" by "obs_operator" ;;
        surf10) surface  "range_upper" vs  "range_lower" by "obs_operator" ;;
        surf11) surface "Taver_length" vs            "c" by "obs_operator" ;;
        surf12) surface "Taver_length" vs            "c" by  "update_mode" ;;
        *     ) echo "No experiment with id $exp_id" 1>&2;;
    esac
}

model_script $@
