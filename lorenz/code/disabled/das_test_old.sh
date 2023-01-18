#!/usr/bin/env bash
#=======================================================================
#> @brief Tests all das routines for sanity.
#> @attention It must be run after every potentially harmfull code change.
#> @note Test runs are configured to be very fast, therefore the
#>       results are obviously not significant.
#=======================================================================
das_test_sanity(){
    funct_opening 4

    export verity=0
#    test_model "L96_1s_pf"
    test_model "L96_2s_mix"

    funct_closing 4
}

test_model(){
    funct_opening 3
    export model=$1

    das_make_all.sh $model
    test_dir=$ARCH_DIR/test/$model;
    rm -rf $test_dir
    source $CODE_DIR/$model/default_config.sh

    run_free_dir=$test_dir/run_free
    run_free.sh "$run_free_dir"; das_stats.sh "$run_free_dir"

    run_assi_dir(){ echo $test_dir/run_assi_$update_mode; }
    (   export update_mode='Hakim'
        run_assi.sh "$(run_assi_dir)" "$run_free_dir"
        das_stats.sh "$(run_assi_dir)"   )

    (   export update_mode='Augm1'
        run_assi.sh "$(run_assi_dir)" "$run_free_dir"
        das_stats.sh "$(run_assi_dir)"   )

    run_free_set_1D_1_dir=$test_dir/run_free_set_1D_1
    run_set.sh -m free "$run_free_set_1D_1_dir" "cycle_length" "arange(5,8)*$dt"
    das_stats.sh "$run_free_set_1D_1_dir"

    run_free_set_1D_2_dir=$test_dir/run_free_set_1D_2
    run_set.sh -m free "$run_free_set_1D_2_dir" "cycle_length" "arange(5,8)*$dt"
    das_stats.sh "$run_free_set_1D_2_dir"

    # Currently runs are not reproducible so diff of 2 identical run sets is not zero
    run_free_set_diff_1D_dir=$test_dir/run_free_set_diff_1D
    run_set_diff.sh "$run_free_set_1D_1_dir" "$run_free_set_1D_2_dir" "$run_free_set_diff_1D_dir"

    run_free_set_2D_1_dir=$test_dir/run_free_set_2D_1
    run_set.sh -m free "$run_free_set_2D_1_dir" \
        "cycle_length" "arange(6,8)*$dt" "cycles" "{10..12}"
    das_stats.sh "$run_free_set_2D_1_dir"

    run_free_set_2D_2_dir=$test_dir/run_free_set_2D_2
    run_set.sh -m free "$run_free_set_2D_2_dir" \
        "cycle_length" "arange(6,8)*$dt" "cycles" "{10..12}"
    das_stats.sh "$run_free_set_2D_2_dir"

    run_free_set_diff_2D_dir=$test_dir/run_free_set_diff_2D
    run_set_diff.sh "$run_free_set_2D_1_dir" "$run_free_set_2D_2_dir" "$run_free_set_diff_2D_dir"

    run_assi_set_1D_dir=$test_dir/run_assi_set_1D
    run_set.sh -m assi -r "$run_free_dir" "$run_assi_set_1D_dir" \
        "SNR" "linspace(2,10,2)"
    das_stats.sh "$run_assi_set_1D_dir"

    run_assi_set_2D_1_dir=$test_dir/run_assi_set_1_2D
    run_set.sh -m assi -r "$run_free_set_1D_1_dir" "$run_assi_set_2D_1_dir" \
        "SNR" "linspace(2,10,2)"
    das_stats.sh "$run_assi_set_2D_1_dir"

    run_assi_set_2D_2_dir=$test_dir/run_assi_set_2_2D
    run_set.sh -m assi -r "$run_free_set_2D_1_dir" "$run_assi_set_2D_2_dir"
    das_stats.sh "$run_assi_set_2D_2_dir"

#    rm -rf $test_dir

#    cp $sampling_dir/config/model_config.cfg .
#    sampling_run.sh $(sampling_dir) $config_file $verity
#    read verity < $sampling_dir/config/verity.cfg
    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh
das_test_sanity $@
exit $?
