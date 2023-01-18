#!/usr/bin/env bash
#=======================================================================
# L63 model default configuration
#=======================================================================

# Sampling parameters
export sampling_size=20  # Hardwired in the fortran code
export            dt="0.01"

# Trajectory parameters
export           t0="0.0"
cycle_steps=5; product $dt $cycle_steps cycle_length;
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
    # 1)  export sampling_period=500
    #     export   spinup_cycles=100
    #     export          cycles=1000
    # 	export     cycles_span="ispan($spinup_cycles + 100,$cycles,100)"
    1)  export sampling_period=500
        export   spinup_cycles=500
        export          cycles=10000
        # export          cycles=5000
    	export     cycles_span="ispan($spinup_cycles + 500,$cycles,500)"
        ;;
    *) error "Unknown verity level $verity";;
esac

# Observation operator parameters
export obs_operator="iden"
export          SNR="10.0"

# Filter
export          filter="enkf"
export       infl_mode="step_" # inflation mode ("step_","cycle","post_")
export       infl_enkf="1.01"
export infl_cycle_length_scaling="const"

# export          filter="letkf"
# export          xlocal="100.0" # no effective localization
# export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
# export infl_factor_ini="1.01"
# export      local_mode="fixed" # localization mode ('fixed','adapt','both_')

# Time-averaged updating mode
export     update_mode="Hakim"
