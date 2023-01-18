#!/usr/bin/env bash
source "$MODEL_DIR/model_tools.sh"
#=======================================================================
# L63_2s model configuration
#=======================================================================

model_config(){
    default_config
    set_pars
    set_inflation
    model_update_code

    addi_info=$(par_id "ensemble_size" "Taver_span_size" "obs_operator")

    # addi_info=""
    # addi_info="${addi_info}__m${ensemble_size}"
    # addi_info="${addi_info}__Taver_range-${Taver_span_size}"
    # addi_info="${addi_info}__obs_operator-${obs_operator}"
    # # addi_info="${addi_info}__compLoc_${comp_localization}"
    # # addi_info="${addi_info}__eta_${eta}"
    # # addi_info="${addi_info}__update_mode-${update_mode}"

    exp_dir(){ echo "$STORE/$model/$par_set/${exp_name}__${addi_info}";}

    # if [[ $clone == yes ]];then
    #     create_code_copy "$(exp_dir)"
    #     cd "$(exp_dir)/code"
    #     source ./initialize_das.sh
    # fi

    # [[ $build == yes ]] && ( das_make_all $model )
}

default_config(){
    #----------------------
    # Sampling parameters
    #----------------------
    export sampling_size=61 # Should be larger than ensemble size
    case "$verity" in
        0)  export sampling_period=50 ;; # in model steps
        *)  export sampling_period=5000 ;;
    esac

    #-------------------------------
    # Trajectory parameters
    #-------------------------------
    export     t0="0.0"
    export     dt="0.01"

    cycle_steps=40
    product $dt $cycle_steps cycle_length
    export cycle_length

#    export   Taver_mode="tied"

    export   Taver_mode="loose";
    Taver_steps=40; product $dt $Taver_steps Taver_length
    export Taver_length

    case "$verity" in
        0)  cycles_step=20    ;;  # for test plots
        #1)  cycles_step=100   ;;  # for surface plots
        #1)  cycles_step=500  ;;  # for surface plots
        1)  cycles_step=1000  ;;  # for surface plots
        #2)  cycles_step=2500 ;;  # for line plots
        #1)  cycles_step=10000 ;;  # for line plots
        2)  cycles_step=10000 ;;  # for line plots
#        2)  cycles_step=20000 ;;  # for line plots
        *) error "Unknown verity level $verity";;
    esac
    export   spinup_cycles=$cycles_step
    export          cycles=$((cycles_step * 10))
    export     cycles_span="ispan($spinup_cycles + $cycles_step,$cycles,$cycles_step)"

    #----------------------------------
    # Observation operator parameters
    #----------------------------------
    obs_operators[0]='identity'
    obs_operators[1]='Norm_add'
    #obs_operators[3]='Resp_add'
    #obs_operators[4]='Resp_comp1'
    #obs_operators[5]='Resp_comp2'
    #obs_operators[6]='Resp_min'
    #obs_operators[7]='Resp_product'
    #obs_operators[8]='Resp_lukasiewicz'
    #obs_operators[9]='Resp_yager'

    #obs_operators[10]='comp1'
    #obs_operators[11]='comp2'
    #obs_operators[12]='comp_add'

    export obs_operator=${obs_operators[0]}
    export          SNR="10.0"
    export          eta="0.1" # slow-fast mix parameter (0:slow - 1:fast)
    export  range_upper="3.0" # in standard deviations
    export  range_lower="3.0" # in standard deviations
#     export    sat_level="0.0"

    #-------------------------------
    # Filter
    #-------------------------------
    export         filter="enkf"
    export  ensemble_size=20
    export      infl_mode="cycle" # inflation mode ("step_","cycle","post_")
    export infl_cycle_length_scaling="none"
    export         comp_localization="no"

    #-------------------------------
    # Time-averaged updating mode
    #-------------------------------
    update_modes[0]='Hakim'
    update_modes[1]='Insta'
    update_modes[2]='Augm0'
    update_modes[3]='Augm4'
    export update_mode=${update_modes[0]}

    #-------------------------------
    # Diagnostic Statistics
    #-------------------------------
    export diag="Xsel"
    export line_detailed_stats="no"

    #-----------------------------------------
    # Parameter spans for sensitivity studies
    #-----------------------------------------
    declare -A Taver_span
    Taver_span[small]="ispan(1,101,10)*$dt"
    Taver_span[mid]="ispan(1,301,30)*$dt"
    Taver_span[big]="ispan(1,1001,100)*$dt"

    span[Taver_length]="${Taver_span[$Taver_span_size]}"
    span[cycle_length]="ispan(1,101,10)*$dt"
    #span[update_mode]='(/"Hakim","Augm1","Augm3","Insta"/)'
    #span[update_mode]='(/"Hakim","Augm1","Insta","Augm4"/)'
    span[update_mode]='(/"Hakim","Insta","Augm0","Augm4"/)'
    span[infl_enkf]="fspan(1.0,1.2,10)"
    span[obs_operator]='(/"Resp_add","Resp_min","Resp_product","Resp_lukasiewicz","Resp_yager"/)'
    #span[obs_operator]='(/"ident","ident_uncoupled"/)'
    span[eta]="fspan(0.0,1.0,5)"
    span[loc_radius]="fspan(1,6,6)"
    span[sat_level]="fspan(0,0.99,10)"
    span[infl_mode]='(/"step_","cycle"/)'
    span[SNR]="fspan(0.1,10.0,10)"
    span[S]="fspan(1,10,10)"
    span[k1]="fspan(1,10,10)"
    span[tao]="fspan(0.1,1.0,10)"
    span[range_upper]="fspan(0.1,4.0,10)"
    span[range_lower]="fspan(0.1,4.0,10)"
    span[c]="fspan(0.0,0.5,11)"
    span[comp_localization]='(/"yes","no"/)'
    span[dt]='(/0.01,0.005,0.002,0.001/)'


    #-------------------------------------------------
    # Old overly complicated inflation configuration
    #-------------------------------------------------
    # # export     infl_mode="step_" # inflation mode ("step_","cycle","post_")
    # case $infl_mode in
    #     "cycle")
    #         export       infl_enkf="1.1"
    #         export infl_cycle_length_scaling="none"
    #         inflation_span="fspan(1.0,1.2,10)"
    #         ;;
    #     "step_")
    #         export infl_cycle_length_scaling="none"
    #         # export infl_cycle_length_scaling="const"
    #         # export infl_cycle_length_scaling="sqrt"
    #         case $infl_cycle_length_scaling in
    #             "none")  # - normal infl setting
    #                 export       infl_enkf="1.04"
    #                 inflation_span="fspan(1.0,1.1,10)"
    #                 ;;
    #             "const") # infl_step = infl_enkf**(1.0d0/cycle_steps)
    #                 export       infl_enkf="1.04"
    #                 inflation_span="fspan(1.0,1.3,10)"
    #                 ;;
    #             "sqrt")  # infl_enkf = (infl_enkf*SQRT(cycle_steps-1.0d0)) + 1.0d0
    #                 export      infl_enkf="0.016"
    #                 inflation_span="fspan(0.005,0.04,10)"
    #                 ;;
    #             *) error "Unknown infl_cycle_length_scaling $infl_cycle_length_scaling";;

    #         esac
    #         ;;
    #     *) error "Unknown infl_mode $infl_mode";;
    # esac
    # export          filter="letkf"
    # export          xlocal="100.0" # no effective localization
    # export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
    # export infl_factor_ini="1.01"
    # export      local_mode="fixed" # localization mode ('fixed','adapt','both_')
}

#=======================================================================
# @brief L63_2s dynamical model configurations
#=======================================================================
set_pars(){
    case "$par_set" in
        "uncoupled")
            isotropic_coupling="yes"; c="0.00"; S="1.0"; k1="10.0";;

        # Original Pena & Kalnay coupling configurations
        "weather_convection")
            isotropic_coupling="no";  c="0.15"; cz="0.0"; S="0.1"; k1="10.0" ;;
        "extratropical_Ocen_atm_strong_coupling")
            isotropic_coupling="no";  c="0.15"; cz="0.0"; S="1.0"; k1="10.0" ;;
        "extratropical_Ocen_atm_weak_coupling")
            isotropic_coupling="no";  c="0.08"; cz="0.0"; S="1.0"; k1="10.0" ;;
        "enso")
            isotropic_coupling="yes"; c="1.00"; S="1.0"; k1="-11.0";;

        # Tödler et al coupling configurations for shallow-deep soil layers
        "shallow-deep_soil_weak_coupling")
            isotropic_coupling="yes"; c="0.1"; S="8.0"; k1="10.0";;
        "shallow-deep_soil_weak_coupling_minus")
            isotropic_coupling="yes"; c="0.1"; S="-8.0"; k1="10.0";;
        "shallow-deep_soil_mid_coupling")
            isotropic_coupling="yes"; c="0.5"; S="8.0"; k1="10.0";;
        "shallow-deep_soil_strong_coupling")
            isotropic_coupling="yes"; c="0.9"; S="8.0"; k1="10.0";;

        # My new configurations
        "enso-c0.00")
            isotropic_coupling="yes"; c="0.00"; S="1.0"; k1="-11.0";;
        "enso-c0.10")
            isotropic_coupling="yes"; c="0.10"; S="1.0"; k1="-11.0";;
        "enso-c0.20")
            isotropic_coupling="yes"; c="0.20"; S="1.0"; k1="-11.0";;
        "enso-c0.22")
            isotropic_coupling="yes"; c="0.22"; S="1.0"; k1="-11.0";;
        "enso-c0.23")
            isotropic_coupling="yes"; c="0.23"; S="1.0"; k1="-11.0";;
        "enso-c0.24")
            isotropic_coupling="yes"; c="0.24"; S="1.0"; k1="-11.0";;
        "enso-c0.25")
            isotropic_coupling="yes"; c="0.25"; S="1.0"; k1="-11.0";;
        "enso-c0.30")
            isotropic_coupling="yes"; c="0.30"; S="1.0"; k1="-11.0";;
        "enso-c0.40")
            isotropic_coupling="yes"; c="0.40"; S="1.0"; k1="-11.0";;
        "enso-c0.50")
            isotropic_coupling="yes"; c="0.50"; S="1.0"; k1="-11.0";;
        "enso-c0.55")
            isotropic_coupling="yes"; c="0.55"; S="1.0"; k1="-11.0";;
        "enso-c0.60")
            isotropic_coupling="yes"; c="0.60"; S="1.0"; k1="-11.0";;
        "enso-c0.70")
            isotropic_coupling="yes"; c="0.70"; S="1.0"; k1="-11.0";;
        "enso-c0.90")
            isotropic_coupling="yes"; c="0.90"; S="1.0"; k1="-11.0";;
        "enso-c1.00")
            isotropic_coupling="yes"; c="1.00"; S="1.0"; k1="-11.0";;
        *) error "Unknown par_set $par_set";;
    esac

    export isotropic_coupling
    export c; export cz; # x-y, z Coupling strengths
    export S;            # Amplitude scale factor
    export k1;           # Offset (uncentering parameter)
}

model_config
