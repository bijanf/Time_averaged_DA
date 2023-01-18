#!/usr/bin/env bash
source "$MODEL_DIR/model_tools.sh"
#=======================================================================
# L96_2s_pf model configuration
#=======================================================================

model_config(){
    default_config
    set_pars
    set_inflation
    model_update_code

    addi_info=""
    addi_info="${addi_info}__m${ensemble_size}"
    addi_info="${addi_info}__Taver_range-${Taver_range}"
    addi_info="${addi_info}__Resp_window_width-${range_upper}"
    #addi_info="${addi_info}__obs_operator-${obs_operator}"
    # addi_info="${addi_info}__update_mode-${update_mode}"

    exp_dir(){ echo "$STORE/$model/$par_set/${exp_name}${addi_info}";}
}

default_config(){
    #-----------------------
    # Sampling parameters
    #-----------------------
    export sampling_size=41 # Should larger than ensemble size
    case "$verity" in
        0)  export sampling_period=50 ;; # in model steps
        *)  export sampling_period=5000 ;;
    esac

    #--------------------------
    # Trajectory parameters
    #--------------------------
    export   t0="0.0"
    export   dt="0.01" # model time step

    cycle_steps=200
    product $dt $cycle_steps cycle_length
    export cycle_length

    # export   Taver_mode="tied"

    export   Taver_mode="loose";
    Taver_steps=200; product $dt $Taver_steps Taver_length
    export Taver_length

    case "$verity" in
        0)  cycles_step=10    ;;  # for test plots
        #0)  cycles_step=100    ;;  # for test plots
        1)  cycles_step=500  ;;  # for surface plots
        2)  cycles_step=5000 ;;  # for line plots
        *) error "Unknown verity level $verity";;
    esac
    export   spinup_cycles=$cycles_step
    export          cycles=$((cycles_step * 10))
    export     cycles_span="ispan($spinup_cycles + $cycles_step,$cycles,$cycles_step)"

    #----------------------------------
    # Observation operator parameters
    #----------------------------------
    obs_operators[0]='Resp_add'
    obs_operators[1]='Resp_comp1'
    obs_operators[2]='Resp_comp2'
    obs_operators[3]='Resp_min'
    obs_operators[4]='Resp_product'
    obs_operators[5]='Resp_lukasiewicz'
    obs_operators[6]='Resp_yager'

    export obs_operator=${obs_operators[0]}
    export          SNR="10.0"
    export          eta="0.5" # slow-fast mix parameter (0:slow-1:fast) 
    #export  range_upper="1.5" # in standard deviations
    #export  range_lower="1.5" # in standard deviations
    export  range_upper="3.0" # in standard deviations
    export  range_lower="3.0" # in standard deviations
    # export    sat_level="0.0" # for "vsl0" & "vsl1"

    #--------
    # Filter
    #--------
    export        filter="enkf"
    export  ensemble_size=20
    export     infl_enkf="1.0"
    export     infl_mode="cycle" # inflation mode ("step_","cycle","post_")
    export infl_cycle_length_scaling="none"
    export    loc_radius="2.0"
    
    #------------------------------
    # Time-averaged updating mode
    #------------------------------
    export     update_mode="Hakim"
    #export     update_mode="Augm0"

    #------------------------------
    # Diagnostic Statistics
    #------------------------------
    export diag="Fmean"
    export line_detailed_stats="no"

    #-----------------------------------------
    # Parameter spans for sensitivity studies
    #-----------------------------------------
    declare -A Taver_span
    # Taver_span[small]="ispan(1,51,5)*$dt"
    Taver_span[mid]="ispan(1,201,20)*$dt"
    Taver_span[big]="ispan(1,401,40)*$dt"
    Taver_range="big"
    
    span[Taver_length]="${Taver_span[$Taver_range]}"

    span[cycle_length]="ispan(1,101,10)*$dt"
    span[update_mode]='(/"Hakim","Insta","Augm0","Augm4"/)'
    span[infl_enkf]="fspan(1.0,1.2,10)"
    span[obs_operator]='(/"Resp_add","Resp_min","Resp_product","Resp_lukasiewicz","Resp_yager"/)'
    span[loc_radius]="fspan(1,6,6)"
    #span[sat_level]="fspan(0,0.99,10)"
    span[infl_mode]='(/"step_","cycle"/)'
    span[range_upper]="fspan(0.01,4.0,10)"
    span[range_lower]="fspan(0.01,4.0,10)"
    span[c]="fspan(0.05,1,10)"
    span[b]="fspan(0.5,5.0,10)"

    #-------------------------------------------------
    # Previous too complicated inflation configuration
    #case $infl_mode in
        #"cycle")
            #export       infl_enkf="1.1"
            #export infl_cycle_length_scaling="none"
            #inflation_span="fspan(1.0,1.2,10)"
            #;;
        #"step_")
            #export infl_cycle_length_scaling="none"
            ## export infl_cycle_length_scaling="const"
            ## export infl_cycle_length_scaling="sqrt"
            #case $infl_cycle_length_scaling in
                #"none")  # - normal infl setting
                    #export       infl_enkf="1.04"
                    #inflation_span="fspan(1.0,1.1,10)"
                    #;;
                #"const") # infl_step = infl_enkf**(1.0d0/cycle_steps)
                    #export       infl_enkf="1.04"
                    #inflation_span="fspan(1.0,1.3,10)"
                    #;;
                #"sqrt")  # infl_enkf = (infl_enkf*SQRT(cycle_steps-1.0d0)) + 1.0d0
                    #export      infl_enkf="0.016"
                    #inflation_span="fspan(0.005,0.04,10)"
                    #;;
                #*) error "Unknown infl_cycle_length_scaling $infl_cycle_length_scaling";;

            #esac
            #;;
        #*) error "Unknown infl_mode $infl_mode";;
    #esac
    #-------------------------------------------------

    #-------------------------------------------------
    # Currently disabled Letkf filter
    # export          filter="letkf"
    # export          xlocal="100.0" # no effective localization
    # export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
    # export infl_factor_ini="1.01"
    # export      local_mode="fixed" # localization mode ('fixed','adapt','both_')
    #-------------------------------------------------

}

#=======================================================================
# @brief L96_2s_pf dynamical model configurations
#=======================================================================
set_pars(){

    constant_forcing(){ F_mean="8.0d0"; F_ampl="0.0d0"; F_tau="6.0d0"; }
    strong_forcing(){ F_mean="12.0d0"; F_ampl="4.0d0"; F_tau="6.0d0"; }
    mild_forcing  (){ F_mean="12.0d0"; F_ampl="2.0d0"; F_tau="6.0d0"; }
    #strong_forcing
    constant_forcing
    
    case "$par_set" in
        "c0.5_h0.5_b0.5")
            c="0.5"; h="0.5"; b="0.5";;
        "c0.5_h0.5_b1.0")
            c="0.5"; h="0.5"; b="1.0";;
        "c0.5_h0.5_b4.0")
            c="0.5"; h="0.5"; b="4.0";;
        "c0.5_h1.0_b1.0")
            c="0.5"; h="1.0"; b="1.0";;

        "c0.1_h0.5_b0.5")
            c="0.1"; h="0.5"; b="0.5";;
        "c0.5_h1.0_b1.0")
            c="0.5"; h="1.0"; b="1.0";;
        "c1.0_h0.3_b-1.0")
            c="1.0"; h="0.3"; b="-1.0";;
        "c1.0_h0.3_b1.0")
            c="1.0"; h="0.3"; b="1.0";;
            
#         "c0.5-h4.0-b4.0")
#             c="0.5"; h="4.0"; b="4.0";   infl_enkf="1.0";;
#         "c0.2-h4.0-b4.0")
#             c="0.2"; h="4.0"; b="4.0";   infl_enkf="1.0";;
#         "c0.5_h4.0_b-4.0")
#             c="0.5"; h="4.0"; b="-4.0";  infl_enkf="1.0";;
#         "c0.4_h4.0_b-4.0")
#             c="0.4"; h="4.0"; b="-4.0";  infl_enkf="1.0";;
#         "c0.5_h1.0_b-4.0")
#             c="0.5"; h="1.0"; b="-4.0";  infl_enkf="1.0";;
#         "c1.0_h1.0_b4.0")
#             c="1.0"; h="1.0"; b="4.0";  infl_enkf="1.0";;
#         "c1.0_h1.0_b-4.0")
#             c="1.0"; h="1.0"; b="-4.0";  infl_enkf="1.0";;
#         "c1.0_h0.5_b4.0")
#             c="1.0"; h="0.5"; b="4.0";  infl_enkf="1.0";;
        *) error "Unknown par_set $par_set";;
    esac

    export     c    # Comp. 2 time scale factor
    export     h    # Coupling constant
    export     b    # Comp. 2 inverse amplitude factor
    export F_mean   # Sinusoidal Forcing parameters
    export F_ampl
    export F_tau
}

model_config
