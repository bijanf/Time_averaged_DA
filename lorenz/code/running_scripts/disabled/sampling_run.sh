#!/usr/bin/env bash
sampling_run(){
    funct_opening 3
    local dataset_dir=$1
    local config_file=$2
    local      verity=$3

    # Create dataset dir
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir

    # Create and report metainfo
    dataset_kind="$FUNCNAME";
    fortran_program=sampling.exe
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "dataset_dir"
    set_par_file "config_file"
    set_par_file "verity"
    set_par_file "fortran_program"
    print_line 0

    # Bring necessary files
    cp $config_file model_config.cfg
    cp $WKDIR/$fortran_program .

    # Set model enviroment
    source model_config.cfg $verity

    # launch fortran code
    case "$debug" in
        "on" ) ddd $fortran_program ;;
        "off")   ./$fortran_program ;;
        *) error "Unknown debug flag $debug";;
    esac

    mkdir config; mv *.cfg config
    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
#source   ${MODEL_DIR}/model_das_tools.sh

sampling_run $@
exit $?

