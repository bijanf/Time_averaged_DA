#!/usr/bin/env bash
#=======================================================================
# L96_1s_pf model default configuration
#=======================================================================

# Sampling parameters
export sampling_size=20  # Hardwired in the fortran code
export            dt="0.01"

# Model parameters
export F_mean="8.0d0"
export F_ampl="0.0d0"
export  F_tau="1.0d0" # Meaningless for F_ampl=0

# Trajectory parameters
export     t0="0.0"
cycle_steps=1; product $dt $cycle_steps cycle_length
export cycle_length

# export   Taver_mode="tied"
export   Taver_mode="loose";
Taver_steps=1; product $dt $Taver_steps Taver_length
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
    	export     cycles_span="ispan($spinup_cycles + 500,$cycles,500)"
        ;;
    *) error "Unknown verity level $verity";;
esac

# Observation operator parameters
export obs_operator="iden"
export          SNR="10.0"

# TA Kalman filter
# export          filter="letkf"
# export          xlocal="2.0" # no effective localization
# export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
# export infl_factor_ini="1.01"
# export      local_mode="fixed" # localization mode ('fixed','adapt','both_')

export     filter="enkf"
export  infl_mode="step_" # inflation mode ("step_","cycle","post_")
export  infl_enkf="1.01"
export loc_radius="6.0"
export infl_cycle_length_scaling="const"

export     update_mode="Hakim"

# mem_per_cpu=2.4  # calc
# mem_per_cpu=9.2   # stat
# export    mem_per_cpu=17.2 # stat
# max=$(echo "print max(80, 100, 1000)"| python)

export    mem_per_cpu=200 # Mb
