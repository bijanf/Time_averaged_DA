#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create nature and free ensemble runs
#------------------------------------------------------
run_free(){
    funct_opening 3
    [[ $# -eq 1 ]] || error "Usage: Incorrect # of arguments"

    local  dataset_dir=$1
#    local sampling_dir=$2

    create_archive_dir $dataset_dir
    cd $dataset_dir

    # Configure run
    dataset_kind="run";
    dataset_dim=0

    # report and save metainfo
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "dataset_dim"
    set_par_file "dataset_dir"
    set_par_file "verity"
    print_line 0

    # Bring necessary files
#    ln -s $sampling_dir/sample* . # Initial conditions
    cp $WKDIR/sampling_run.exe .      # fortran executable
    cp $WKDIR/nature_run.exe .        # fortran executable
    cp $WKDIR/ensemble_run.exe .  # fortran executable

    launch_executable sampling_run.exe

    launch_executable nature_run.exe

    export run_mode="free"
    launch_executable ensemble_run.exe

    # Tidy up dataset
    find . -maxdepth 1 -type l | xargs rm -f # delete soft links
    rm *.exe                                 # delete executables
    mkdir config; mv *.cfg config            # gather config files
    mkdir raw_data; mv *.* raw_data          # gather the rest

    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh

run_free $@
exit $?
