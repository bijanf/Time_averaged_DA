#!/usr/bin/env bash
#=======================================================================
# L63_2s model default configuration
#=======================================================================

# Sampling parameters
export sampling_size=20 # Hardwired in the fortran code

set_pars(){
    local par_set=$1
    case "$par_set" in
        "uncoupled")
            c="0.00"; cz="0.0"; S="1.0"; k1="10.0"
            infl_enkf="1.065"
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

#set_pars "extratropical_Ocen_atm_strong_coupling"
set_pars "shallow-deep_soil_mid_coupling"
export     dt="0.01"

# Trajectory parameters
export     t0="0.0"
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
    #1)  export sampling_period=5000
        #export   spinup_cycles=5000
        #export          cycles=100000
      #export     cycles_span="ispan($spinup_cycles + 5000,$cycles,5000)"
        #;;
    1)  export sampling_period=5000
        export   spinup_cycles=500
        export          cycles=10000
        export     cycles_span="ispan($spinup_cycles + 500,$cycles,500)"
        ;;
    # 1)  export sampling_period=5000
    #     export   spinup_cycles=5000
    #     export          cycles=50000
    #     export     cycles_span="ispan($spinup_cycles + 5000,$cycles,5000)"
    #     ;;
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
export       infl_enkf="1.04"
export infl_cycle_length_scaling="const"

# export          filter="letkf"
# export          xlocal="100.0" # no effective localization
# export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
# export infl_factor_ini="1.01"
# export      local_mode="fixed" # localization mode ('fixed','adapt','both_')

# Time-averaged updating mode
export     update_mode="Hakim"
