#!/usr/bin/env bash

spinup_dir  (){ echo "$(exp_dir)/spinup_run__start${spinup_start}__${spinup_length}${spinup_length_unit}";}
sampling_dir(){ echo "$(exp_dir)/sampling_run__${sampling_step}${sampling_step_unit}_m${sampling_size}";}

length_dir  (){ echo "$(exp_dir)/run_length_${run_length}${run_length_unit}";}

natu_run_dir (){ echo "$(length_dir)/natu_run";}
free_run_dir (){ echo "$(length_dir)/free_run__m${ensemble_size}";}
assi_run_norm_addObs_dir(){ echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500__stationSet${station_set_number}__obs_op_norm_add";}
assi_run_dir (){ echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500__stationSet${station_set_number}__obs_op_${obs_operator}";}


natu_run_set_dir(){ echo "$(length_dir)/natu_run_set";}
free_run_set_dir(){ echo "$(length_dir)/free_run_set__m${ensemble_size}";}
assi_run_set_dir(){ echo "$(length_dir)/assi_run_set__m${ensemble_size}__stationSet${station_set_number}__obs_op_${obs_operator}";}

#addi_info       (){ echo "";}
#addi_info       (){ echo "$(addi_info)__m${ensemble_size}";}
#free_run_set_dir(){ echo "$(length_dir)/free_run_set__$(addi_info)";}
#addi_info       (){ echo "$(addi_info)__stationSet${station_set_number}";}
#addi_info       (){ echo "$(addi_info)__obs_op_${obs_operator}";}
#assi_run_set_dir(){ echo "$(length_dir)/assi_run_set__$(addi_info)";}

#nature_run_set_dir(){ echo "$(exp_dir)/nature_run_set__${cycles}cycles";}
#free_run_set_dir  (){ echo "$(exp_dir)/free_run_set__${cycles}cycles";}

init_runs(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"

    run.sh --run_type=spinup     "$(spinup_dir)" || error

    run.sh --run_type=sampling \
        --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
}

single_runs(){
    funct_opening 4

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"

    run.sh --run_type=nature \
        --spinup_dir="$(spinup_dir)" \
        "$(natu_run_dir)" || error

    run.sh --run_type=free \
        --sampling_dir="$(sampling_dir)" \
        --nature_dir="$(natu_run_dir)" \
        "$(free_run_dir)" || error

    run.sh --run_type=assi \
        --free_run_dir="$(free_run_dir)" \
        "$(assi_run_dir)" || error
}

line(){
    funct_opening 4

    set_par1_name=$1;
    #[[ $2 == "by" ]] || error "Invalid syntax"
    #par1_name=$3; par2_name=${4:-};

    export verity=${verity:-1}
    source "$MODEL_DIR/model_config.sh"
    
    [[ $set_par1_name == "cycle_length" ]] || \
        error "Unsupported set_par1_name $set_par1_name"

    #prefix="line_${set_par1_name}"
    #exp_name="${prefix}"

    #prefix="line_${set_par1_name}__Taver_mode-tied__by"
    #if [[ -n $par2_name ]]; then
        #exp_name="${prefix}_${par1_name}_${par2_name}"
    #else
        #exp_name="${prefix}_${par1_name}"
    #fi

     run_set.sh --run_type=nature \
         --spinup_dir="$(spinup_dir)" \
         --par1_name=$set_par1_name \
         --par1_span=${span[$set_par1_name]} \
         --par1_unit=${unit[$set_par1_name]} \
         "$(natu_run_set_dir)" || error
 
     run_set.sh --run_type=free \
         --sampling_dir="$(sampling_dir)" \
         --ref_set_dir="$(natu_run_set_dir)" \
         "$(free_run_set_dir)" || error

    run_set.sh --run_type=assi \
        --ref_set_dir="$(free_run_set_dir)" \
        "$(assi_run_set_dir)" || error
}

# line(){
#     funct_opening 4
# 
#     set_par1_name=$1;
#     #[[ $2 == "by" ]] || error "Invalid syntax"
#     #par1_name=$3; par2_name=${4:-};
# 
#     export verity=${verity:-1}
#     source "$MODEL_DIR/model_config.sh"
#     
#     [[ $set_par1_name == "cycle_length" ]] || \
#         error "Unsupported set_par1_name $set_par1_name"
# 
#     #prefix="line_${set_par1_name}"
#     #exp_name="${prefix}"
# 
#     #prefix="line_${set_par1_name}__Taver_mode-tied__by"
#     #if [[ -n $par2_name ]]; then
#         #exp_name="${prefix}_${par1_name}_${par2_name}"
#     #else
#         #exp_name="${prefix}_${par1_name}"
#     #fi
# 
#     run_set.sh --run_type=nature \
#         --spinup_dir="$(spinup_dir)" \
#         --par1_name=$set_par1_name \
#         --par1_span=${span[$set_par1_name]} \
#         --par1_unit=${unit[$set_par1_name]} \
#         "$(natu_run_set_dir)" || error
# 
#     run_set.sh --run_type=free \
#         --sampling_dir="$(sampling_dir)" \
#         --ref_set_dir="$(natu_run_set_dir)" \
#         "$(free_run_set_dir)" || error
# 
#     run_set.sh --run_type=assi \
#         --ref_set_dir="$(free_run_set_dir)" \
#         "$(assi_run_set_dir)" || error
# }
