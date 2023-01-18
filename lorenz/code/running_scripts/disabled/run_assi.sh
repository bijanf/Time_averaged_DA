#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create ensemble with data assimilation
#------------------------------------------------------
run_assi(){
    funct_opening 3
    [[ $# -eq 2 ]] || error "Usage: Incorrect # of arguments"

    local  dataset_dir=$1
    local run_free_dir=$2
    [[ -d "$run_free_dir" ]] || error "Non existent run_free_dir $run_free_dir"

    create_archive_dir $dataset_dir
    cd $dataset_dir

    # Configure run
    dataset_kind="run";
    rel_run_free_dir=$(relpath $run_free_dir $dataset_dir )

    # report and save metainfo
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "dataset_dir"
    set_par_file "run_free_dir"
    set_par_file "rel_run_free_dir"
    set_par_file "verity"
    print_line 0

    # Bring necessary files
    ln -s $rel_run_free_dir/raw_data/sampling.nc . # Initial conditions
    ln -s $rel_run_free_dir/raw_data/nature_Insta_all.nc . # nature_run
    cp $WKDIR/ensemble_run.exe .      # fortran executable

    # Run fortran code
    export run_mode="assi"
    # ddd ensemble_run.exe
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
#source   ${MODEL_DIR}/model_das_tools.sh

run_assi $@
exit $?
