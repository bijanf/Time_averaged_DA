#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source tools/common_tools.sh
source initialize_system.sh
#=======================================================================
#> @brief Tests all das routines for sanity.
#> @attention It must be run after every potentially harmfull code change.
#> @note Results are obviously not significant.
#=======================================================================
build=no
das_test(){
    echo "========================="
    echo "  Checking DAS' sanity"
    echo "========================="
    build=${build:-yes}

    [[ $build == yes ]] && das_make_all.sh

    echo " Removing previous test output"
    rm -fr $STORE/test*

    spinup_dir(){ echo "$STORE/test_spinup_run";}
    run.sh --run_type=spinup --time_start=1979010100 \
        --cycle_length=24 $(spinup_dir)

    run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
        --cycle_length=6 --cycles=4 "$STORE/test_nature_run"

    run_set.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
        --par1=cycle_length --span1="(/24,168,336,672/)" \
        "$STORE/test_nature_run_set"

    return 0

    # sampling_dir(){ echo "$STORE/test_sampling_run";}
    # run.sh --run_type=nature --cycle_length=24 --cycles=20 \
    #   --spinup_dir="$(spinup_dir)" "$(sampling_dir)"

    # yes | spinup.sh             1979010100 $((24*1)) test_spinup $ARCHIVE_DIR

#       yes | nature_run.sh         test_spinup 6 3 test_nature
    # yes | nature_run.sh         test_spinup 12 2 test_nature

    # yes | sampling.sh           test_spinup 6 20 test_sampling
        #yes | free_ensemble_run.sh  test_free_run test_nature test_sampling 20 yes
    yes | free_ensemble_run.sh  test_free_run test_nature test_sampling 20 no

#       yes | assim_ensemble_run.sh "test_assim_run_Origi_Insta" "test_free_run" "station_homogeneous_gap2" "Origi" "Insta"

    yes | assim_ensemble_run.sh "test_assim_run_Origi_Taver" "test_free_run" "station_homogeneous_gap2" "Origi" "Taver"

#       export SNR="2.0"
#       yes | assim_ensemble_run.sh "test_assim_run_Stemp_Insta" "test_free_run" "station_homogeneous_gap2" "STemp" "Insta"

    export SNR="2.0"
    yes | assim_ensemble_run.sh "test_assim_run_Stemp_Taver" "test_free_run" "station_homogeneous_gap2" "STemp" "Taver"

        #plot_nature_run.sh
        #plot_free_ensemble_run.sh
    echo "========================="
    echo "  Succesfull test"
    echo "========================="
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

das_test $@
exit $?
