#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $DAS_DIR/common_tools.sh
task=${task:-check}
source $CODE_DIR/initialize_das.sh
source $CODE_DIR/models/$model/default_config.sh
#=======================================================================
#> @brief List of runs DAS should perform succesfully to prove sanity.
#=======================================================================
test_list(){
    funct_opening 5
    model=$1
    verity=${verity:-0}
    verbose=${verbose:-1}
    source $CODE_DIR/models/$model/default_config.sh
    test_dir=$ARCH_DIR/test/$model;

    if [[ $global_calc_flag == yes ]]; then
        echo " Removing previous test output"
        rm -rf $test_dir
    fi

    test_routines=( \
        test_individual_runs \
        test_run_free_set1D \
        test_run_free_set2D \
        test_run_assi_set1D \
        #test_run_assi_set2D \
        test_run_assi_set_bunch1D \
            test_run_assi_set_bunch2D \
            )


    rm -f test.log
    for test_routine in ${test_routines[@]}; do
        echo " - $test_routine"
        case $verbose in
	    0)
		echo " To check the progress execute"
		echo " tail -f test.log"
		$test_routine >> test.log || error;;
            1) $test_routine || error | tee test.log 2>&1;;
            *) error "Unsupported verbosity level $verbose";;
        esac
    done

#    rm -rf $test_dir
    funct_closing 5
}

test_individual_runs(){
    funct_opening 4

    run_free_dir=$test_dir/run_free
    bash -x run.sh --run_mode=free --detailed_stats=yes --verbose=yes \
        "$run_free_dir" || error

    run_assi_dir(){ echo $test_dir/run_assi_$update_mode; }

    update_mode='Hakim'; run_assi_dir_A="$(run_assi_dir)"
    run.sh --run_mode=assi --detailed_stats=yes \
        --ref_dir="$run_free_dir" \
        "$run_assi_dir_A" || error

    update_mode='Augm1'; run_assi_dir_B="$(run_assi_dir)"
    run.sh --run_mode=assi --detailed_stats=yes \
        --ref_dir="$run_free_dir" \
        "$run_assi_dir_B" || error

    # update_mode='Augm2'; run_assi_dir_B="$(run_assi_dir)"
    # run.sh --run_mode=assi --ref_dir="$run_free_dir" \
    #     "$run_assi_dir_B" || error

    # run_assi_diff_dir="$test_dir/run_assi_diff"
    # run_diff.sh --run_A="$run_assi_dir_A" --run_B="$run_assi_dir_B" \
    #     "$run_assi_diff_dir" || error

    funct_closing 4
}

test_run_free_set1D(){
    funct_opening 4

    #run_free_set1D_1_dir=$test_dir/run_free_set1D_1
    #run_set.sh --run_mode=free\
        #--par1=cycle_length --span1="ispan(5,8,1)*$dt" \
        #"$run_free_set1D_1_dir" || error
    #return 0

    run_free_set1D_1_dir=$test_dir/run_free_set1D_1
    run_set.sh --run_mode=free --detailed_stats=yes --run_verbose=no \
        --par1=cycle_length --span1="ispan(5,8,1)*$dt" \
        "$run_free_set1D_1_dir" || error

    run_free_set1D_2_dir=$test_dir/run_free_set1D_2
    run_set.sh --run_mode=free \
        --par1=cycle_length --span1="ispan(5,8,1)*$dt" \
        "$run_free_set1D_2_dir" || error

    #-----------------------------------------------
    # Currently runs are not reproducible
    # so diff of 2 identical run sets is not zero:
    #-----------------------------------------------
    # run_free_set1D_diff_dir=$test_dir/run_free_set1D_diff
    # run_set_diff.sh \
    #     --set_A="$run_free_set1D_1_dir" \
    #     --set_B="$run_free_set1D_2_dir" \
    #     "$run_free_set1D_diff_dir" || error

    funct_closing 4
}

test_run_assi_set1D(){
    funct_opening 4

    update_mode='Hakim'; run_assi_set1D_1_dir=$test_dir/run_assi_set1D_1
    run_set.sh --run_mode=assi --ref_dir="$run_free_dir" \
        --par1=SNR --span1="fspan(2,10,2)" \
        --detailed_stats=yes \
        "$run_assi_set1D_1_dir" || error

    update_mode='Augm1'; run_assi_set1D_2_dir=$test_dir/run_assi_set1D_2
    run_set.sh --run_mode=assi --ref_dir="$run_free_dir" \
        --par1=SNR --span1="fspan(2,10,2)" \
        "$run_assi_set1D_2_dir" || error

    # run_assi_set1D_diff_dir=$test_dir/run_assi_set1D_diff
    # run_set_diff.sh \
    #     --set_A="$run_assi_set1D_1_dir" \
    #     --set_B="$run_assi_set1D_2_dir" \
    #     "$run_assi_set1D_diff_dir" || error

    funct_closing 4
}

test_run_free_set2D(){
    funct_opening 4

    run_free_set2D_1_dir=$test_dir/run_free_set2D_1
    run_set.sh --run_mode=free \
        --par1=cycle_length --span1="ispan(6,8,1)*$dt" \
        --par2=Taver_length --span2="ispan(1,3,2)*$dt" \
        "$run_free_set2D_1_dir" || error

    funct_closing 4
}

test_run_assi_set2D(){
    funct_opening 4

    run_assi_set2D_1_dir=$test_dir/run_assi_set2D_1
    run_set.sh --run_mode=assi --ref_dir="$run_free_set1D_1_dir" \
        --par1=SNR --span1="fspan(2,10,8)" \
        "$run_assi_set2D_1_dir" || error

    run_assi_set2D_2_dir=$test_dir/run_assi_set2D_2
    run_set.sh --run_mode=assi --ref_dir="$run_free_set2D_1_dir" \
        "$run_assi_set2D_2_dir" || error

    funct_closing 4
}

test_run_assi_set_bunch1D(){
    funct_opening 4

    run_free_set1D_1_dir=$test_dir/run_free_set1D_1
    run_free_set2D_1_dir=$test_dir/run_free_set2D_1

    run_assi_set1D_bunch1D_dir=$test_dir/run_assi_set1D_bunch1D
    run_set_bunch.sh --run_mode=assi \
        --ref_dir="$run_free_set1D_1_dir" \
        --bunch_par1=update_mode \
        --bunch_span1='(/"Hakim","Augm1"/)' \
        "$run_assi_set1D_bunch1D_dir" || error

    run_assi_set2D_bunch1D_dir=$test_dir/run_assi_set2D_bunch1D
    run_set_bunch.sh --run_mode=assi \
        --ref_dir="$run_free_set2D_1_dir" \
        --bunch_par1=update_mode \
        --bunch_span1='(/"Hakim","Augm1"/)' \
        "$run_assi_set2D_bunch1D_dir" || error

    funct_closing 4
}

test_run_assi_set_bunch2D(){
    funct_opening 4

    run_free_set1D_1_dir=$test_dir/run_free_set1D_1
    run_free_set2D_1_dir=$test_dir/run_free_set2D_1

    run_assi_set1D_bunch2D_dir=$test_dir/run_assi_set1D_bunch2D
    run_set_bunch.sh --run_mode=assi \
        --ref_dir="$run_free_set1D_1_dir" \
        --bunch_par1=update_mode \
        --bunch_span1='(/"Hakim","Augm1","Augm2"/)' \
        --bunch_par2=SNR \
        --bunch_span2="fspan(2,10,2)"\
        "$run_assi_set1D_bunch2D_dir" || error

    run_assi_set2D_bunch2D_dir=$test_dir/run_assi_set2D_bunch2D
    run_set_bunch.sh --run_mode=assi \
        --ref_dir="$run_free_set2D_1_dir" \
        --bunch_par1=update_mode \
        --bunch_span1='(/"Hakim","Augm1","Augm2"/)' \
        --bunch_par2=SNR \
        --bunch_span2="fspan(2,10,2)"\
        "$run_assi_set2D_bunch2D_dir" || error

    funct_closing 4
}

test_list $@
exit $?
