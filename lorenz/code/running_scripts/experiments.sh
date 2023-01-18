#!/usr/bin/env bash

run_free_dir          (){ echo "$(exp_dir)/free-run__$run_free_info";}
run_assi_dir          (){ echo "$(exp_dir)/assi-run__$run_assi_info";}
run_free_set_dir      (){ echo "$(exp_dir)/free_run_set"; }
run_assi_set_dir      (){ echo "$(exp_dir)/assi_run_set"; }
run_assi_set_bunch_dir(){ echo "$(exp_dir)/assi_run_set_bunch"; }


single_run(){
    funct_opening 4

    cycle_length_in="$1"
    Taver_length_in="$2"
    [[ $3 == "by" ]] || error "Invalid syntax"
    set_par1_name=$4;

    export verity=${verity:-1}

    prefix="run__Taver_length-${Taver_length_in}__cycle_length-${cycle_length_in}"
    exp_name="${prefix}__by_${set_par1_name}"

    source "$MODEL_DIR/model_config.sh"
    cycle_length="$cycle_length_in"
    Taver_length="$Taver_length_in"

    Taver_mode="loose";
    run_free_info="";

    # model_make
    
    run.sh --run_mode=free --keep_raw_data=yes "$(run_free_dir)" || error

    run_set.sh --run_mode=assi --calc=yes --stat=yes \
        --par1=${set_par1_name:-} --span1=${span[$set_par1_name]} \
        --keep_raw_data=yes \
        --detailed_stats=${detailed_stats:-} \
        --ref_dir="$(run_free_dir)" \
        "$(run_assi_set_dir)"

    funct_closing 4
}

# By default  Taver_mode="tied"
line(){
    funct_opening 4

    set_par1_name=$1;
    [[ $2 == "by" ]] || error "Invalid syntax"
    bunch_par1_name=$3; bunch_par2_name=${4:-};

    export verity=${verity:-2}

    prefix="line_${set_par1_name}__Taver_mode-tied__by"
    if [[ -n $bunch_par2_name ]]; then
        exp_name="${prefix}_${bunch_par1_name}_and_${bunch_par2_name}"
    else
        exp_name="${prefix}_${bunch_par1_name}"
    fi

    source "$MODEL_DIR/model_config.sh"
    Taver_mode="tied"

    bunch_span1="${span[$bunch_par1_name]}"
    if [[ -n $bunch_par2_name ]]; then
        bunch_span2="${span[$bunch_par2_name]}"
    else
        bunch_span2=""
    fi

    run_set.sh --run_mode=free \
        --detailed_stats=$line_detailed_stats \
        --par1="$set_par1_name" --span1="${span[$set_par1_name]}" \
        "$(run_free_set_dir)" || error

    run_set_bunch.sh --run_mode=assi \
        --detailed_stats=$line_detailed_stats \
        --ref_dir="$(run_free_set_dir)" \
        --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
        --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
        --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

    funct_closing 4
}

# By default  Taver_mode="tied", unless set_par1_name=Taver_length & set_par2_name=cycle_length
surface(){
    funct_opening 4

    set_par1_name=$1;
    [[ $2 == "vs" ]] || error "Invalid syntax"
    set_par2_name=$3;
    [[ $4 == "by" ]] || error "Invalid syntax"
    bunch_par1_name=$5;
    bunch_par2_name=${6:-};

    verbose=${verbose:-1}
#     [[ $set_par1_name == "Taver_length" ]] || error "Unsupported set_par1_name $set_par1_name"

    declare -A par_kind

    for par_name in $set_par1_name $set_par2_name; do
        case "$par_name" in
            Taver_length|cycle_length|S|F_ampl|c|b|dt)
                par_kind[$par_name]=free_par;;
            infl_enkf|SNR|range_upper|range_lower|comp_localization|eta)
                par_kind[$par_name]=assi_par;;
            *    )
                error "Unknown parameter ${set_par2_name}";;
        esac
    done


    export verity=${verity:-1}
#    prefix="surf_${set_par1_name}_vs_${set_par2_name}__Taver_mode-tied__by"
    prefix="surf_${set_par1_name}_vs_${set_par2_name}__by"
    if [[ -n $bunch_par2_name ]]; then
        exp_name="${prefix}_${bunch_par1_name}_and_${bunch_par2_name}"
    else
        exp_name="${prefix}_${bunch_par1_name}"
    fi

    source "$MODEL_DIR/model_config.sh"

    Taver_mode="tied"
    if [[ $set_par1_name == "Taver_length" ]] && [[ $set_par2_name == "cycle_length" ]]; then
        Taver_mode="loose"
    fi


    bunch_span1="${span[$bunch_par1_name]}"
    if [[ -n $bunch_par2_name ]]; then
        bunch_span2="${span[$bunch_par2_name]}"
    else
        bunch_span2=""
    fi

    if  [[ ${par_kind[$set_par1_name]} == free_par ]] && [[ ${par_kind[$set_par2_name]} == free_par ]]; then
        run_set.sh --run_mode=free \
            --detailed_stats=$line_detailed_stats \
            --par1="${set_par1_name}" --span1="${span[$set_par1_name]}" \
            --par2="${set_par2_name}" --span2="${span[$set_par2_name]}" \
            "$(run_free_set_dir)" || error

        run_set_bunch.sh --run_mode=assi \
            --detailed_stats=$line_detailed_stats \
            --ref_dir="$(run_free_set_dir)" \
            --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
            --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
            --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

    elif [[ ${par_kind[$set_par1_name]} == free_par ]] && [[ ${par_kind[$set_par2_name]} == assi_par ]]; then
        run_set.sh --run_mode=free \
            --detailed_stats=$line_detailed_stats \
            --par1="${set_par1_name}" --span1="${span[$set_par1_name]}" \
            "$(run_free_set_dir)" || error

        run_set_bunch.sh --run_mode=assi \
            --detailed_stats=$line_detailed_stats \
            --ref_dir="$(run_free_set_dir)" \
            --set_par1="${set_par2_name}"    --set_span1="${span[$set_par2_name]}" \
            --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
            --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
            --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

    elif [[ ${par_kind[$set_par1_name]} == assi_par ]] && [[ ${par_kind[$set_par2_name]} == free_par ]]; then
        run_set.sh --run_mode=free \
            --detailed_stats=$line_detailed_stats \
            --par1="${set_par2_name}" --span1="${span[$set_par2_name]}" \
            "$(run_free_set_dir)" || error

        run_set_bunch.sh --run_mode=assi \
            --detailed_stats=$line_detailed_stats \
            --ref_dir="$(run_free_set_dir)" \
            --set_par1="${set_par1_name}"    --set_span1="${span[$set_par1_name]}" \
            --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
            --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
            --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

    elif [[ ${par_kind[$set_par1_name]} == assi_par ]] && [[ ${par_kind[$set_par2_name]} == assi_par ]]; then
        run.sh --run_mode=free \
            --detailed_stats=$line_detailed_stats \
            "$(run_free_set_dir)" || error

        run_set_bunch.sh --run_mode=assi \
            --detailed_stats=$line_detailed_stats \
            --ref_dir="$(run_free_set_dir)" \
            --set_par1="${set_par1_name}"    --set_span1="${span[$set_par1_name]}" \
            --set_par2="${set_par2_name}"    --set_span2="${span[$set_par2_name]}" \
            --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
            --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
            --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
    else
        error "par_kind determination problem"
    fi

    funct_closing 4
}


surf_Inst-Obs__Taver_length(){
    funct_opening 4


    [[ $1 == "vs" ]] || error "Invalid syntax"
    set_par2_name=$2;
    [[ $3 == "by" ]] || error "Invalid syntax"
    bunch_par1_name=$4;
    bunch_par2_name=${5:-};

    case "$set_par2_name" in
        S)
par_kind=free_par;;
infl_enkf|SNR)
par_kind=assi_par;;
*    )
error "Unknown parameter ${set_par2_name}";;
esac

export verity=${verity:-1}
prefix="surf_Taver_length_vs_${set_par2_name}__Inst-Obs__by"
if [[ -n $bunch_par2_name ]]; then
    exp_name="${prefix}_${bunch_par1_name}_${bunch_par2_name}"
else
    exp_name="${prefix}_${bunch_par1_name}"
fi

source "$MODEL_DIR/model_config.sh"
Taver_mode="loose"; Taver_length="$dt";

bunch_span1="${span[$bunch_par1_name]}"
if [[ -n $bunch_par2_name ]]; then
    bunch_span2="${span[$bunch_par2_name]}"
else
    bunch_span2=""
fi

case "$par_kind" in
    free_par)
    run_set.sh --run_mode=free \
        --detailed_stats=$line_detailed_stats \
        --par1="Taver_length" --span1="${span[Taver_length]}" \
        --par2="${set_par2_name}" --span2="${span[$set_par2_name]}" \
        "$(run_free_set_dir)" || error

    run_set_bunch.sh --run_mode=assi \
        --detailed_stats=$line_detailed_stats \
        --ref_dir="$(run_free_set_dir)" \
        --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
        --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
        --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
    ;;
    assi_par)
    run_set.sh --run_mode=free \
        --detailed_stats=$line_detailed_stats \
        --par1="Taver_length" --span1="${span[Taver_length]}" \
        "$(run_free_set_dir)" || error

    run_set_bunch.sh --run_mode=assi \
        --detailed_stats=$line_detailed_stats \
        --ref_dir="$(run_free_set_dir)" \
        --set_par1="${set_par2_name}" --set_span1="${span[$set_par2_name]}" \
        --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
        --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
        --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
    ;;
    *    )
    error "Unknown par_kind $par_kind";;
esac


funct_closing 4
}


# line_InstantaneousObs__by_update_mode__obs_operator(){
#     funct_opening 4
#
#     export verity=${verity:-2}
#     source "$MODEL_DIR/model_config.sh"
#     exp_info="cycle_length-$(cycle_length_span)"
#     exp_name="${FUNCNAME}_${exp_info}"
#
#     Taver_mode="loose"; Taver_length="$dt";
#
#     run_set.sh --run_mode=free --detailed_stats=yes \
#         --par1="cycle_length" --span1="$(cycle_length_span)" \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh --run_mode=assi --detailed_stats=yes \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="$exp_name" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }


# line_Taver_length__Taver_mode-tied__by_parameter(){
#     funct_opening 4
#
#     par1_name=$1; par2_name=${2:-};
#
#     export verity=${verity:-2}
#     prefix="line_Taver_length__Taver_mode-tied__by"
#     if [[ -n $par2_name ]]; then
#         exp_name="${prefix}_${par1_name}_${par2_name}"
#     else
#         exp_name="${prefix}_${par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#     Taver_mode="tied"
#
#     bunch_span1="${span[$par1_name]}"
#     if [[ -n $par2_name ]]; then
#         bunch_span2="${span[$par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="${span[Taver_length]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="$par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#

# surface(){
# #surface_Taver_length_vs_cycle_length__by_parameter(){
#     funct_opening 4
#
#     set_par1_name=$1;
#     [[ $2 == "vs" ]] || error "Invalid syntax"
#     set_par2_name=$3;
#     [[ $4 == "by" ]] || error "Invalid syntax"
#     bunch_par1_name=$5;
#     bunch_par2_name=${6:-};
#
#     export verity=${verity:-1}
#     prefix="surf_${set_par1_name}_vs_${set_par2_name}__by"
#     if [[ -n $bunch_par2_name ]]; then
#         exp_name="${prefix}_${bunch_par1_name}_${bunch_par2_name}"
#     else
#         exp_name="${prefix}_${bunch_par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#
#     if [[ $set_par1_name == "Taver_length" ]] && [[ $set_par2_name == "cycle_length" ]] then
#   Taver_mode="loose"
#     else
#   Taver_mode="tied"
#     fi
#
#     bunch_span1="${span[$bunch_par1_name]}"
#     if [[ -n $bunch_par2_name ]]; then
#         bunch_span2="${span[$bunch_par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="$set_par1_name" --span1="${span[$set_par1_name]}" \
#         --par2="$set_par2_name" --span2="${span[$set_par2_name]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="$bunch_par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$bunch_par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }


# line_Taver_mode-tied__Taver_length(){
#     funct_opening 4
#
#     [[ $1 == "by" ]] || error "Invalid syntax"
#     par1_name=$2; par2_name=${3:-};
#
#     export verity=${verity:-2}
#     prefix="line_Taver_length__Taver_mode-tied__by"
#     if [[ -n $par2_name ]]; then
#         exp_name="${prefix}_${par1_name}_${par2_name}"
#     else
#         exp_name="${prefix}_${par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#     Taver_mode="tied"
#
#     bunch_span1="${span[$par1_name]}"
#     if [[ -n $par2_name ]]; then
#         bunch_span2="${span[$par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="${span[Taver_length]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="$par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#

# single_run(){
#     funct_opening 4
#
#     cycle_length="$1"
#     Taver_length="$2"
#     export verity=${verity:-1}
#     exp_info="Taver_length-${Taver_length}__cycle_length-${cycle_length}"
#     exp_name="${FUNCNAME}_$exp_info"
#
#     source "$MODEL_DIR/model_config.sh"
#
#     Taver_mode="loose";
#
#     run_free_info="";
#
#     run.sh --run_mode=free "$(run_free_dir)" || error
#
#     for update_mode in Hakim Augm1 Augm3 Insta; do
#       run_assi_info="update_mode-${update_mode}"
#         run.sh --run_mode=assi --ref_dir="$(run_free_dir)" \
#             --report_name="$run_assi_info" --keep_raw_data=yes \
#             "$(run_assi_dir)" &
#     done
#
#     funct_closing 4
# }


# surf_Taver_length_vs_infl__Taver_mode-tied__by_parameter(){
#     funct_opening 4
#
#     par1_name=$1; par2_name=${2:-};
#
#     export verity=${verity:-1}
#     prefix="surf_Taver_length_vs_infl__Taver_mode-tied__by"
#     if [[ -n $par2_name ]]; then
#         exp_name="${prefix}_${par1_name}_${par2_name}"
#     else
#         exp_name="${prefix}_${par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#     Taver_mode="tied"
#
#     bunch_span1="${span[$par1_name]}"
#     if [[ -n $par2_name ]]; then
#         bunch_span2="${span[$par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="${span[Taver_length]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --set_par1="infl_enkf"        --set_span1="${span[infl_enkf]}" \
#         --bunch_par1="$par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#
# surf_Taver_length_vs_infl__Inst-Obs___by_parameter(){
#     funct_opening 4
#
#     par1_name=$1; par2_name=${2:-};
#
#     export verity=${verity:-1}
#     prefix="surf_Taver_length_vs_infl__Inst-Obs__by"
#     if [[ -n $par2_name ]]; then
#         exp_name="${prefix}_${par1_name}_${par2_name}"
#     else
#         exp_name="${prefix}_${par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#     Taver_mode="loose"; Taver_length="$dt";
#
#     bunch_span1="${span[$par1_name]}"
#     if [[ -n $par2_name ]]; then
#         bunch_span2="${span[$par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="cycle_length" --span1="${span[cycle_length]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --set_par1="infl_enkf"        --set_span1="${span[infl_enkf]}" \
#         --bunch_par1="$par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#
# surf_Taver_length_vs_cycle_length__by_parameter(){
#     funct_opening 4
#
#     par1_name=$1; par2_name=${2:-};
#
#     export verity=${verity:-1}
#     prefix="surf_Taver_length_vs_cycle_length__by"
#     if [[ -n $par2_name ]]; then
#         exp_name="${prefix}_${par1_name}_${par2_name}"
#     else
#         exp_name="${prefix}_${par1_name}"
#     fi
#
#     source "$MODEL_DIR/model_config.sh"
#     Taver_mode="loose"
#
#     bunch_span1="${span[$par1_name]}"
#     if [[ -n $par2_name ]]; then
#         bunch_span2="${span[$par2_name]}"
#     else
#         bunch_span2=""
#     fi
#
#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="${span[Taver_length]}" \
#         --par2="cycle_length" --span2="${span[cycle_length]}" \
#         "$(run_free_set_dir)" || error
#
#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="$par1_name"  --bunch_span1="$bunch_span1" \
#         --bunch_par2="$par2_name"  --bunch_span2="$bunch_span2" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#

# surface_InstantaneousObs_vs_inflation(){
#     funct_opening 4
#
#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#
#     Taver_mode="loose"; Taver_length="$dt";
#     exp_info=""
#
#     run_set.sh --run_mode=free \
#         --par1=cycle_length --span1=$(cycle_length_span) \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh --run_mode=assi --ref_dir="$(run_free_set_dir)" \
#         --set_par1="infl_enkf"        --set_span1="$inflation_span" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="$(exp_name)" "$(run_assi_set_bunch_dir)" || error
#         # --bunch_par2=infl_mode   --bunch_span2=$infl_mode_span \
#
#     funct_closing 4
# }
#


# surface_Taver_length_vs_cycle_length__by_update_mode__obs_operator(){
#     funct_opening 4
#
#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#     exp_info="Taver_length-$(Taver_length_span)__cycle_length-$(cycle_length_span)"
#     exp_name="${FUNCNAME}"
#
#     Taver_mode="loose"; filter="enkf"
#
#     run_set.sh --run_mode=free \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         --par2="cycle_length" --span2="$(cycle_length_span)" \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh  --run_mode=assi --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="$exp_name" "$(run_assi_set_bunch_dir)" || error
#
#     funct_closing 4
# }
#
#
# surface_Taver_mode-tied_vs_infl__by_update_mode__obs_operator(){
#     funct_opening 4

#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#     #exp_info="Taver_length-$(Taver_length_span)_infl_enkf-${inflation_span}"
#     exp_name="$FUNCNAME"

#     Taver_mode="tied";

#     run_set.sh --run_mode=free \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh --run_mode=assi --ref_dir="$(run_free_set_dir)" \
#         --set_par1="infl_enkf"        --set_span1="$inflation_span" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="$exp_name" "$(run_assi_set_bunch_dir)" || error
#         #--bunch_par2=infl_mode   --bunch_span2=$infl_mode_span \

#     funct_closing 4
# }

# surface_Taver_mode-tied_vs_sat_level__by_update_mode__obs_operator(){
#     funct_opening 4

#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#     #exp_info="Taver_length-$(Taver_length_span)_infl_enkf-${inflation_span}"
#     exp_name="$FUNCNAME"

#     Taver_mode="tied";

#     run_set.sh --run_mode=free \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh --run_mode=assi --ref_dir="$(run_free_set_dir)" \
#         --set_par1="sat_level"        --set_span1="$sat_level_span" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="$exp_name" "$(run_assi_set_bunch_dir)" || error
#         #--bunch_par2=infl_mode   --bunch_span2=$infl_mode_span \

#     funct_closing 4
# }

# surface_Taver_mode-tied_vs_infl__by_loc_radius(){
#     funct_opening 4

#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#     exp_info="Taver_length-$(Taver_length_span)_infl_enkf-${inflation_span}"
# #    exp_name="${FUNCNAME}_$exp_info"
#     exp_name="${FUNCNAME}"

#     Taver_mode="tied";

#     run_set.sh --run_mode=free \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         "$(run_free_set_dir)" || error
#     run_set_bunch.sh --run_mode=assi --ref_dir="$(run_free_set_dir)" \
#         --set_par1="infl_enkf"        --set_span1="$inflation_span" \
#         --bunch_par1="obs_operator" --bunch_span1='(/"plus"/)' \
#         --bunch_par2="loc_radius"   --bunch_span2="$loc_radius_span"  \
#         --report_name="$exp_name" "$(run_assi_set_bunch_dir)" || error

#     funct_closing 4
# }


# line_Taver_mode-tied__by_update_mode__obs_operator(){
#     funct_opening 4

#     export verity=${verity:-2}
#     source "$MODEL_DIR/model_config.sh"
#     exp_info="$infl_enkf"
#     exp_name="${FUNCNAME}_$exp_info"

#     Taver_mode="tied"

#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         "$(run_free_set_dir)" || error

#      run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --bunch_par2="obs_operator" --bunch_span2="$obs_operator_span" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

#     funct_closing 4
# }

# line_Taver_mode-tied__by_update_mode(){
#     funct_opening 4

#     export verity=${verity:-2}
#     source "$MODEL_DIR/model_config.sh"
#     exp_info="$infl_enkf"
#     exp_name="${FUNCNAME}_$exp_info"

#     Taver_mode="tied"

#     run_set.sh --run_mode=free \
#         --detailed_stats=$line_detailed_stats \
#         --par1="Taver_length" --span1="$(Taver_length_span)" \
#         "$(run_free_set_dir)" || error

#     run_set_bunch.sh --run_mode=assi \
#         --detailed_stats=$line_detailed_stats \
#         --ref_dir="$(run_free_set_dir)" \
#         --bunch_par1="update_mode"  --bunch_span1="$update_mode_span" \
#         --report_name="${exp_name}" "$(run_assi_set_bunch_dir)" || error

#     funct_closing 4
# }





# single_runs(){
#     funct_opening 4

#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"

#     exp_name=${exp_name:-$FUNCNAME}
#     run_free_id (){ echo "Taver_length-${Taver_length}__cycle_length-${cycle_length}";}
#     run_assi_id (){ echo "update_mode-${update_mode}__$(run_free_id)";}
#     run_free_dir(){ echo "$(exp_dir)/free-run__$(run_free_id)";}
#     run_assi_dir(){ echo "$(exp_dir)/assi-run__$(run_assi_id)";}

#     sampling_period=5000;

#     Taver_mode="loose";

#     # assimilating Instantaneous Obs
#     cycle_length="0.10"; Taver_length="0.01"
#     run.sh --run_mode=free "$(run_free_dir)" || error
#     update_mode="Hakim"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error
#     update_mode="Augm1"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error
#     update_mode="Augm2"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error

#     # assimilating Time-averaged Obs
#     # cycle_length="0.10"; Taver_length="0.10"
#     cycle_length="0.30"; Taver_length="0.30"
#     run.sh --run_mode=free "$(run_free_dir)" || error
#     update_mode="Hakim"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error
#     update_mode="Augm1"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error
#     update_mode="Augm2"
#     run.sh --run_mode=assi --ref_dir="$(run_free_dir)" --report_name="$(run_assi_id)" \
#         "$(run_assi_dir)" || error

#     funct_closing 4
# }
