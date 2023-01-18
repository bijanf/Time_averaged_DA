#!/usr/bin/env bash
#=======================================================================
# L96_2s_mix model configuration
#=======================================================================

model_config(){
    default_config
    set_pars
    
    if [[ $clone == yes ]];then
        create_code_copy "$(exp_dir)"
        cd "$(exp_dir)/code"
        source ./initialize_das.sh
    fi

    [[ $build == yes ]] && ( das_make_all $model )
}

default_config(){
    #-----------------------
    # Sampling parameters
    #-----------------------
    export sampling_size=41 # Should larger than ensemble size    export sampling_size=40  # Hardwired in the fortran code
    case "$verity" in
        0)  export sampling_period=50 ;; # in model steps
        *)  export sampling_period=5000 ;;
    esac

    #--------------------------
    # Trajectory parameters
    #--------------------------
    export t0="0.0"
    export dt="0.005"

    cycle_steps=5;
    product $dt $cycle_steps cycle_length
    export cycle_length

    # export   Taver_mode="tied"
    
    export   Taver_mode="loose";
    Taver_steps=1; product $dt $Taver_steps Taver_length
    export Taver_length

    case "$verity" in
        0)  cycles_step=10    ;;  # for test plots
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
    obs_operators=("plus" "vsl0" "vsl1")
    export obs_operator=${obs_operators[0]}
    export          SNR="10.0"
    export    sat_level="0.0" # for "vsl0" & "vsl1"

    #--------
    # Filter
    #--------
    export        filter="enkf"
    export     infl_mode="cycle" # inflation mode ("step_","cycle","post_")
    export infl_cycle_length_scaling="none"
    export    loc_radius="2.0" 

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
    # export     update_mode="Hakim"
    # export          xlocal="2.0" # no effective localization
    # export       infl_mode="fixed" #    inflation mode ('fixed' or 'adapt')
    # export infl_factor_ini="1.01"
    # export      local_mode="fixed" # localization mode ('fixed','adapt','both_')
    #-------------------------------------------------

    #------------------------------
    # Time-averaged updating mode
    #------------------------------
    export     update_mode="Hakim"

    #------------------------------
    # Diagnostic Statistics
    #------------------------------
    export diag="Fmean"

    #-----------------------------------------
    # Parameter spans for sensitivity studies
    #-----------------------------------------
    span[Taver_length]="ispan(1,101,10)*$dt"
    span[cycle_length]="ispan(1,101,5)*$dt"
    span[update_mode]='(/"Hakim","Insta"/)'
    #span[update_mode]='(/"Hakim","Augm1","Augm3","Insta"/)'
    span[infl_enkf]="fspan(1.0,1.2,10)"
    span[obs_operator]='(/"addi","plus","vsl0","vsl1"/)'
    span[loc_radius]="fspan(1,6,6)"
    span[sat_level]="fspan(0,0.99,10)"
    span[infl_mode]='(/"step_","cycle"/)'
    span[loc_radius]="fspan(1,10,10)"
#     #Taver_length_span(){ echo "ispan(1,51,5)*$dt";}
#     #cycle_length_span(){ echo "ispan(1,51,5)*$dt";}
#     #update_mode_span='(/"Hakim","Augm1","Augm2"/)'
}

#=======================================================================
# @brief L96_2s_mix dynamical model configurations
#=======================================================================
set_pars(){
    case "$par_set" in
        "link0.0")
            link="0.0d0"
            infl_enkf="1.022"
            ;;
        "link0.1")
            link="0.1d0"
            infl_enkf="1.022"
            ;;
        "link0.2")
            link="0.2d0"
            infl_enkf="1.022"
            ;;
        *) error "Unknown par_set $par_set";;
    esac

    export  force="8.0d0"  # Forcing
    export alpha1="1.0d0"  # Component 1 Time scaling constant
    export alpha2="10.0d0" # Component 2 Time scaling constant
    export   link          # Coupling constant [0,0.35)
    #                        link=0.4 makes the model crash
    export infl_enkf
}

model_config
