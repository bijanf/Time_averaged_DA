#!/usr/bin/env bash
#set -xv
ens_free_run(){
    funct_opening 3
    dataset_dir=$1
    nature_dir=$2

    create_archive_dir $dataset_dir || return 0

    sampling_dir=$(cat $nature_dir/config/sampling_dir.cfg)
    fortran_program=ensemble_run.exe
    export run_mode="free"
    export   detail=2

    cd $dataset_dir
    ln -s $sampling_dir/sample*.dat .
    ln -s $nature_dir/nature*.fbn .
    cp $WKDIR/$fortran_program .

    case "$debug" in
        "on" ) ddd $fortran_program ;;
        "off")   ./$fortran_program ;;
        *) error "Unknown debug flag $debug";;
    esac

    rm *.exe
    find . -maxdepth 1 -type l | xargs rm -f # delete soft links

    echo "$nature_dir">nature_dir.cfg
    echo "$FUNCNAME" > dataset_kind.cfg
    mkdir config; mv *.cfg config

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
#source   ${MODEL_DIR}/model_das_tools.sh

ens_free_run $@
exit $?
