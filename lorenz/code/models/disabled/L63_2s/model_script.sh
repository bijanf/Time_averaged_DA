#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
source $DAS_DIR/experiments.sh

#=======================================================================
# L63_2s model running script
#=======================================================================
model_script(){
    sim_id=$1
    pars_set_number=0
    verity=3

    check_dependencies
    default_config

    pars_set[0]="uncoupled"
    pars_set[1]="enso"
    pars_set[2]="extratropical_Ocen_atm_strong_coupling"
    pars_set[3]="extratropical_Ocen_atm_weak_coupling"
    pars_set[4]="extratropical_Ocen_atm_weak_coupling"
    pars_set_id=${pars_set[$pars_set_number]}

    set_pars $pars_set_id

    # - normal infl setting
    # exp_dir(){ echo "$STORE/$pars_set_id/$exp_name"; }
    # inflation_span="fspan(1.0,1.3,10)"

    # - experimental infl setting
    exp_dir(){ echo "$STORE/$pars_set_id/${exp_name}__sqrt-scaled-infl"; }
    infl_cycle_length_scaling="sqrt"
    inflation_span="fspan(0.005,0.04,10)"

    # run_experiment $sim_id
    # exp_id=$1
    case $sim_id in
        exp01) single_runs ;;
        exp02) surface_inflation_vs_localization ;;
        exp03) surface_InstantaneousObs_vs_inflation ;;
        exp04) surface_Taver_mode-tied_vs_inflation ;;
        exp05) line_InstantaneousObs ;;
        exp06) line_Taver_mode-tied ;;
        exp07) surface_Taver_length_vs_cycle_length ;;
        *    ) echo "No experiment with id $exp_id" 1>&2;;
    esac

}

default_config(){

    # Sampling parameters
    export sampling_size=20 # Hardwired in the fortran code

    # Trajectory parameters
    export     t0="0.0"
    export     dt="0.01"

    cycle_steps=5
    product $dt $cycle_steps cycle_length
    export cycle_length

    # export   Taver_mode="tied"

    export   Taver_mode="loose";
    Taver_steps=4; product $dt $Taver_steps Taver_length
    export Taver_length

    verity=${verity:-1}
    case "$verity" in
        0)  export sampling_period=50 # in model steps
            export   spinup_cycles=1
            export          cycles=10
            export     cycles_span="ispan($spinup_cycles + 1,$cycles,2)"
            ;;
        1)  export sampling_period=5000
            export   spinup_cycles=500
            export          cycles=10000
            export     cycles_span="ispan($spinup_cycles + 500,$cycles,500)"
            ;;
        2)  export sampling_period=5000
            export   spinup_cycles=5000
            export          cycles=50000
            export     cycles_span="ispan($spinup_cycles + 5000,$cycles,5000)"
            ;;
        3)  export sampling_period=5000
	    cycles_step=20000
            export   spinup_cycles=$cycles_step
            export          cycles=$((cycles_step * 10))
            export     cycles_span="ispan($spinup_cycles + $cycles_step,$cycles,$cycles_step)"
            ;;
        *) error "Unknown verity level $verity";;
    esac

    # Observation operator parameters
    #export obs_operator='iden'
    export obs_operator="plus"
    #export obs_operator="vsl0"
    #export obs_operator="vsl1"
    export          SNR="10.0"

    # Filter
    export          filter="enkf"
    export       infl_mode="step_" # inflation mode ("step_","cycle","post_")
    #export       infl_enkf="1.04"
    #export infl_cycle_length_scaling="const"

    export infl_cycle_length_scaling="sqrt"
    export       infl_enkf="0.016"

    # export          filter="letkf"
    # export          xlocal="100.0" # no effective localization
    # export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
    # export infl_factor_ini="1.01"
    # export      local_mode="fixed" # localization mode ('fixed','adapt','both_')

    # Time-averaged updating mode
    export     update_mode="Hakim"
}

set_pars(){
    local par_set=$1
    case "$par_set" in
        "uncoupled")
            c="0.00"; cz="0.0"; S="1.0"; k1="10.0"
            # infl_enkf="1.065"
            export infl_cycle_length_scaling="sqrt"
            export       infl_enkf="0.016"
            ;;
        # Original Pena & Kalnay coupling configurations
        "weather_convection")
            c="0.15"; cz="0.0"; S="0.1"; k1="10.0" ;;
        "extratropical_Ocen_atm_strong_coupling")
            c="0.15"; cz="0.0"; S="1.0"; k1="10.0"
            infl_enkf="1.04"
            ;;
        "extratropical_Ocen_atm_weak_coupling")
            c="0.08"; cz="0.0"; S="1.0"; k1="10.0" ;;
        "enso")
            c="1.00"; cz="1.0"; S="1.0"; k1="-11.0";;
        # Tödler et al coupling configurations for shallow-deep soil layers
        "shallow-deep_soil_weak_coupling")
            c="0.1" ; cz="0.1"; S="8.0"; k1="10.0";;
        "shallow-deep_soil_mid_coupling")
            c="0.5" ; cz="0.5"; S="8.0"; k1="10.0";;
        "shallow-deep_soil_strong_coupling")
            c="0.9" ; cz="0.9"; S="8.0"; k1="10.0";;
        *) error "Unknown par_set $par_set";;
    esac
    export c; export cz; export S; export k1
}

model_script $@
