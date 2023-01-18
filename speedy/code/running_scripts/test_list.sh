#!/usr/bin/env bash
source $CODE_DIR/speedy_tools/model_tools.sh
#=======================================================================
#> @brief List of runs DAS should perform succesfully to prove sanity.
#=======================================================================
test_list(){
    funct_opening 5

    verity=${verity:-0}
    verbose=${verbose:-1}

    source "$MODEL_DIR/model_config.sh"
    test_dir=$ARCH_DIR/test/$model;

    echo " Removing previous test output"
    rm -rf $test_dir

    ir=0
    test_routines[$ir]="test_spinup_and_sampling"; ((ir+=1))
    test_routines[$ir]="test_individual_runs"    ; ((ir+=1))
    test_routines[$ir]="test_run_sets_1D"        ; ((ir+=1))
        # test_run_free_set2D \
        # test_run_assi_set1D \
        # test_run_assi_set2D \
        # test_run_assi_set_bunch1D \
        # test_run_assi_set_bunch2D \

    echo "========================="
    echo "  Checking DAS' sanity"
    echo "========================="
    echo " To check the progress execute"
    echo " tail -f test.log"

    rm -f test.log
    for test_routine in ${test_routines[@]}; do
        echo " - $test_routine"
        case $verbose in
            0)  echo " To check the progress execute"
                echo " tail -f test.log"
                $test_routine >> test.log || error ;;
            1)  $test_routine ;;
            # 1)  $test_routine || error | tee test.log 2>&1;;
            *)  error "Unsupported verbosity level $verbose";;
        esac
    done

#    rm -rf $test_dir
    funct_closing 5
}

spinup_dir              (){ echo "$test_dir/test_spinup_run";}
sampling_dir            (){ echo "$test_dir/test_sampling_run";}
nature_dir              (){ echo "$test_dir/test_nature_run";}
free_run_dir            (){ echo "$test_dir/test_free_run";}
assi_run_normT_obs_dir  (){ echo "$test_dir/test_assi_run_normT_obs";}
assi_run_normAdd_obs_dir(){ echo "$test_dir/test_assi_run_normAdd_obs";}
assi_run_normMin_obs_dir(){ echo "$test_dir/test_assi_run_normMin_obs";}
nature_run_set_dir      (){ echo "$test_dir/test_nature_run_set";}
free_run_set_dir        (){ echo "$test_dir/test_free_run_set";}
assi_run_set_dir        (){ echo "$test_dir/test_assi_run_set";}

test_spinup_and_sampling(){
    funct_opening 4

    run.sh --run_type=spinup "$(spinup_dir)" || error

    run.sh --run_type=sampling --spinup_dir="$(spinup_dir)" \
        "$(sampling_dir)" || error

    funct_closing 4
}

test_individual_runs(){
    funct_opening 4

    run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
        "$(nature_dir)" || error

    run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
        --keep_members=yes --nature_dir="$(nature_dir)" \
        "$(free_run_dir)" || error

    export obs_operator='norm_T'
    run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
        "$(assi_run_normT_obs_dir)" || error

    export obs_operator='norm_add'
    run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
        --assi_run_normT_obs_dir="$(assi_run_normT_obs_dir)" \
        "$(assi_run_normAdd_obs_dir)" || error

    export obs_operator='norm_min'
    run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
        --assi_run_normT_obs_dir="$(assi_run_normT_obs_dir)" \
        --assi_run_normAdd_obs_dir="$(assi_run_normAdd_obs_dir)" \
        "$(assi_run_normMin_obs_dir)" || error

    funct_closing 4
}

test_run_sets_1D(){
    funct_opening 4

    run_set.sh --run_type="nature" \
        --spinup_dir="$(spinup_dir)" \
        --par1_name=cycle_length \
        --par1_span=${span[cycle_length]} \
        --par1_unit=${unit[cycle_length]} \
        "$(nature_run_set_dir)" || error

    run_set.sh --run_type="free" \
        --sampling_dir="$(sampling_dir)" \
        --ref_set_dir="$(nature_run_set_dir)" \
        "$(free_run_set_dir)" || error

    run_set.sh --run_type="assi" \
        --ref_set_dir="$(free_run_set_dir)" \
        "$(assi_run_set_dir)" || error

    funct_closing 4
}

test_list;

# test_run_assi_set1D(){
#     funct_opening 4

#     update_mode='Hakim'; run_assi_set1D_1_dir=$test_dir/run_assi_set1D_1
#     run_set.sh --run_mode=assi --ref_dir="$run_free_dir" \
#         --par1=SNR --span1="fspan(2,10,2)" \
#         --detailed_stats=yes \
#         "$run_assi_set1D_1_dir" || error

#     update_mode='Augm1'; run_assi_set1D_2_dir=$test_dir/run_assi_set1D_2
#     run_set.sh --run_mode=assi --ref_dir="$run_free_dir" \
#         --par1=SNR --span1="fspan(2,10,2)" \
#         "$run_assi_set1D_2_dir" || error

#     # run_assi_set1D_diff_dir=$test_dir/run_assi_set1D_diff
#     # run_set_diff.sh \
#     #     --set_A="$run_assi_set1D_1_dir" \
#     #     --set_B="$run_assi_set1D_2_dir" \
#     #     "$run_assi_set1D_diff_dir" || error

#     funct_closing 4
# }

# test_run_free_set2D(){
#     funct_opening 4

#     run_free_set2D_1_dir=$test_dir/run_free_set2D_1
#     run_set.sh --run_mode=free \
#         --par1=cycle_length --span1="ispan(6,8,1)*$dt" \
#         --par2=Taver_length --span2="ispan(1,3,2)*$dt" \
#         "$run_free_set2D_1_dir" || error

#     funct_closing 4
# }

# test_run_assi_set2D(){
#     funct_opening 4

#     run_assi_set2D_1_dir=$test_dir/run_assi_set2D_1
#     run_set.sh --run_mode=assi --ref_dir="$run_free_set1D_1_dir" \
#         --par1=SNR --span1="fspan(2,10,8)" \
#         "$run_assi_set2D_1_dir" || error

#     run_assi_set2D_2_dir=$test_dir/run_assi_set2D_2
#     run_set.sh --run_mode=assi --ref_dir="$run_free_set2D_1_dir" \
#         "$run_assi_set2D_2_dir" || error

#     funct_closing 4
# }

# test_run_assi_set_bunch1D(){
#     funct_opening 4

#     run_free_set1D_1_dir=$test_dir/run_free_set1D_1
#     run_free_set2D_1_dir=$test_dir/run_free_set2D_1

#     run_assi_set1D_bunch1D_dir=$test_dir/run_assi_set1D_bunch1D
#     run_set_bunch.sh --run_mode=assi \
#         --ref_dir="$run_free_set1D_1_dir" \
#         --bunch_par1=update_mode \
#         --bunch_span1='(/"Hakim","Augm1"/)' \
#         "$run_assi_set1D_bunch1D_dir" || error

#     run_assi_set2D_bunch1D_dir=$test_dir/run_assi_set2D_bunch1D
#     run_set_bunch.sh --run_mode=assi \
#         --ref_dir="$run_free_set2D_1_dir" \
#         --bunch_par1=update_mode \
#         --bunch_span1='(/"Hakim","Augm1"/)' \
#         "$run_assi_set2D_bunch1D_dir" || error

#     funct_closing 4
# }

# test_run_assi_set_bunch2D(){
#     funct_opening 4

#     run_free_set1D_1_dir=$test_dir/run_free_set1D_1
#     run_free_set2D_1_dir=$test_dir/run_free_set2D_1

#     run_assi_set1D_bunch2D_dir=$test_dir/run_assi_set1D_bunch2D
#     run_set_bunch.sh --run_mode=assi \
#         --ref_dir="$run_free_set1D_1_dir" \
#         --bunch_par1=update_mode \
#         --bunch_span1='(/"Hakim","Augm1","Augm2"/)' \
#         --bunch_par2=SNR \
#         --bunch_span2="fspan(2,10,2)"\
#         "$run_assi_set1D_bunch2D_dir" || error

#     run_assi_set2D_bunch2D_dir=$test_dir/run_assi_set2D_bunch2D
#     run_set_bunch.sh --run_mode=assi \
#         --ref_dir="$run_free_set2D_1_dir" \
#         --bunch_par1=update_mode \
#         --bunch_span1='(/"Hakim","Augm1","Augm2"/)' \
#         --bunch_par2=SNR \
#         --bunch_span2="fspan(2,10,2)"\
#         "$run_assi_set2D_bunch2D_dir" || error

#     funct_closing 4
# }
