#!/usr/bin/env bash

# run_experiment(){
#     exp_id=$1
#     case $exp_id in
#         exp01) single_runs ;;
#         exp02) surface_inflation_vs_localization ;;
#         exp03) surface_InstantaneousObs_vs_inflation ;;
#         exp04) surface_Taver_mode-tied_vs_inflation ;;
#         exp05) line_InstantaneousObs ;;
#         exp06) line_Taver_mode-tied ;;
#         exp07) surface_Taver_length_vs_cycle_length ;;
#         *    ) echo "No experiment with id $exp_id" 1>&2;;
#     esac
# }

single_runs(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"
    
    exp_name=${exp_name:-$FUNCNAME}
    run_free_id (){ echo "Taver_length-${Taver_length}__cycle_length-${cycle_length}";}
    run_assi_id (){ echo "update_mode-${update_mode}__$(run_free_id)";}
    run_free_dir(){ echo "$(exp_dir)/free-run__$(run_free_id)";}
    run_assi_dir(){ echo "$(exp_dir)/assi-run__$(run_assi_id)";}

    sampling_period=5000;

    Taver_mode="loose";

    # assimilating Instantaneous Obs
    cycle_length="0.10"; Taver_length="0.01"
    run.sh --run_mode=free "$(run_free_dir)" || error
    update_mode="Hakim"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error
    update_mode="Augm1"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error
    update_mode="Augm2"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error

    # assimilating Time-averaged Obs
    # cycle_length="0.10"; Taver_length="0.10"
    cycle_length="0.30"; Taver_length="0.30"
    run.sh --run_mode=free "$(run_free_dir)" || error
    update_mode="Hakim"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error
    update_mode="Augm1"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error
    update_mode="Augm2"
    run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
  "$(run_assi_dir)" || error

    funct_closing 4
}

surface_InstantaneousObs_vs_inflation(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"

    
    Taver_mode="loose"; Taver_length="$dt";
    cycle_length_span=${cycle_length_span:-"ispan(1,51,5)*$dt"}
    #update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1"/)'}
    inflation_span=${inflation_span:-"fspan(1.0,1.05,10)"}
    obs_operator_span=${obs_operator_span:-'(/"plus","vsl0","vsl1"/)'}
    # infl_mode_span=${infl_mode_span:-'(/"step_","cycle"/)'}

    exp_name=${exp_name:-$FUNCNAME}
    run_free_set_dir="$(exp_dir)/free_run_set"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

    run_set.sh --run_mode=free \
        --par1=cycle_length --span1=$cycle_length_span \
        "$run_free_set_dir" || error
    run_set_bunch.sh --run_mode=assi --ref_dir="$run_free_set_dir" \
        --set_par1=infl_enkf       --set_span1=$inflation_span \
        --bunch_par1=update_mode --bunch_span1=$update_mode_span \
        --bunch_par2=obs_operator --bunch_span2=$obs_operator_span \
        --report_name=$exp_name "$run_assi_set_dir" || error
        # --bunch_par2=infl_mode   --bunch_span2=$infl_mode_span \

    funct_closing 4
}

line_InstantaneousObs(){
    funct_opening 4

    export verity=${verity:-2}
    source "$MODEL_DIR/model_config.sh"
    
    Taver_mode="loose"; Taver_length="$dt";
    cycle_length_span=${cycle_length_span:-"ispan(1,51,5)*$dt"}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    obs_operator_span=${obs_operator_span:-'(/"plus","vsl0","vsl1"/)'}
    # infl_mode_span=${infl_mode_span:-'(/"step_","cycle"/)'}

    exp_name=${exp_name:-$FUNCNAME}
    run_free_set_dir="$(exp_dir)/free_run_set"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

    run_set.sh --run_mode=free --detailed_stats=yes \
        --par1=cycle_length --span1=$cycle_length_span \
        "$run_free_set_dir" || error
    run_set_bunch.sh --run_mode=assi --detailed_stats=yes \
        --ref_dir="$run_free_set_dir" \
        --bunch_par1=update_mode  --bunch_span1=$update_mode_span \
        --bunch_par2=obs_operator --bunch_span2=$obs_operator_span \
        --report_name=$exp_name "$run_assi_set_dir" || error

    funct_closing 4
}

surface_Taver_mode-tied_vs_inflation(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"
    
    Taver_mode="tied";
    Taver_length_span=${Taver_length_span:-"ispan(1,101,10)*$dt"}
    # Taver_length_span=${Taver_length_span:-"ispan(1,51,5)*$dt"}
    #update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1"/)'}
    inflation_span=${inflation_span:-"fspan(1.0,1.05,10)"}
    obs_operator_span=${obs_operator_span:-'(/"plus","vsl0","vsl1"/)'}
    #infl_mode_span=${infl_mode_span:-'(/"step_","cycle"/)'}

    exp_name=${exp_name:-"${FUNCNAME}_Taver_length-${Taver_length_span}"}
    run_free_set_dir="$(exp_dir)/free_run_set"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

    run_set.sh --run_mode=free \
        --par1=Taver_length --span1=$Taver_length_span \
        "$run_free_set_dir" || error
    run_set_bunch.sh --run_mode=assi --ref_dir="$run_free_set_dir" \
        --set_par1=infl_enkf      --set_span1=$inflation_span \
        --bunch_par1=update_mode  --bunch_span1=$update_mode_span \
        --bunch_par2=obs_operator --bunch_span2=$obs_operator_span \
        --report_name=$exp_name "$run_assi_set_dir" || error
        #--bunch_par2=infl_mode   --bunch_span2=$infl_mode_span \

    funct_closing 4
}

line_Taver_mode-tied(){
    funct_opening 4

    export verity=${verity:-2}
    source "$MODEL_DIR/model_config.sh"

    Taver_mode="tied"
    Taver_length_span=${Taver_length_span:-"ispan(1,51,5)*$dt"}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1"/)'}
    # update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    obs_operator_span=${obs_operator_span:-'(/"plus","vsl0","vsl1"/)'}
    # infl_mode_span=${infl_mode_span:-'(/"step_","cycle"/)'}

    exp_name=${exp_name:-"${FUNCNAME}_${infl_enkf}"}
    run_free_set_dir="$(exp_dir)/free_run_set"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

     # run_set.sh --run_mode=free --detailed_stats=yes \
     run_set.sh --run_mode=free --detailed_stats=no \
         --par1=Taver_length --span1=$Taver_length_span \
         "$run_free_set_dir" || error
    # run_set_bunch.sh --run_mode=assi --detailed_stats=yes \
    run_set_bunch.sh --run_mode=assi --detailed_stats=no \
        --ref_dir="$run_free_set_dir" \
        --bunch_par1=update_mode  --bunch_span1=$update_mode_span \
        --bunch_par2=obs_operator --bunch_span2=$obs_operator_span \
        --report_name=$exp_name "$run_assi_set_dir" || error

    funct_closing 4
}

surface_Taver_length_vs_cycle_length(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"

    Taver_mode="loose"; filter="enkf"
    Taver_length_span=${Taver_length_span:-"ispan(1,51,5)*$dt"}
    cycle_length_span=${cycle_length_span:-"ispan(1,51,5)*$dt"}
    # update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1"/)'}
    obs_operator_span=${obs_operator_span:-'(/"plus","vsl0","vsl1"/)'}

    exp_name=${exp_name:-$FUNCNAME}
    run_free_set_dir="$(exp_dir)/free_run_set"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

    run_set.sh --run_mode=free \
        --par1="Taver_length" --span1="$Taver_length_span" \
        --par2="cycle_length" --span2="$cycle_length_span" \
        "$run_free_set_dir" || error
    run_set_bunch.sh  --run_mode=assi --ref_dir="$run_free_set_dir" \
        --bunch_par1=update_mode --bunch_span1=$update_mode_span \
        --bunch_par2=obs_operator --bunch_span2=$obs_operator_span \
        --report_name=$exp_name "$run_assi_set_dir" || error

    funct_closing 4
}

surface_inflation_vs_localization(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"
    
    Taver_mode="loose";
    cycle_steps=10; cycle_length=$(echo "$dt * $cycle_steps"|bc)
    Taver_steps=10; Taver_length=$(echo "$dt * $Taver_steps"|bc)

    inflation_span=${inflation_span:-"fspan(1.0,1.1,10)"}
    loc_radius_span=${loc_radius_span:-"fspan(1,20,10)"}
    # update_mode_span=${update_mode_span:-'(/"Hakim","Augm1","Augm2"/)'}
    update_mode_span=${update_mode_span:-'(/"Hakim","Augm1"/)'}
    # infl_mode_span=${infl_mode_span:-'(/"step_","cycle"/)'}

    exp_name=${exp_name:-"${FUNCNAME}__Taver_steps-${Taver_steps}_cycle_steps-${cycle_steps}"}
    run_free_dir="$(exp_dir)/free_run"
    run_assi_set_dir="$(exp_dir)/assi_run_set_bunch"

    run.sh --run_mode=free \
        "$run_free_dir" || error
    run_set_bunch.sh --run_mode=assi --ref_dir="$run_free_dir" \
        --set_par1=infl_enkf       --set_span1=$inflation_span  \
        --set_par2=loc_radius      --set_span2=$loc_radius_span  \
        --bunch_par1=update_mode --bunch_span1=$update_mode_span \
        --report_name=$exp_name "$run_assi_set_dir" || error

    funct_closing 4
}
