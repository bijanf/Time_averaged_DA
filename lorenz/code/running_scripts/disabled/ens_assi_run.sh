#!/usr/bin/env bash
#set -xv
ens_assi_run(){
    funct_opening 3
    dataset_dir=$1
    ens_free_dir=$2

    create_archive_dir $dataset_dir || return 0

    nature_dir=$(cat $ens_free_dir/config/nature_dir.cfg)
    sampling_dir=$(cat $nature_dir/config/sampling_dir.cfg)
    fortran_program=ensemble_run.exe
    export run_mode="assi"

    # cd $ens_free_dir;
    # ls *_error.dat | xargs -I % \
    #     ln -s $ens_free_dir/% $dataset_dir/ens_free_%
    cd $dataset_dir
    ln -s $sampling_dir/sample*.dat .
    ln -s $nature_dir/nature*.fbn .
    cp $WKDIR/$fortran_program .

    case "$debug" in
        "on" ) ddd $fortran_program ;;
        "off")   ./$fortran_program ;;
        *) error "Unknown debug flag $debug";;
    esac

    find . -maxdepth 1 -type l | xargs rm -f # delete soft links
    rm $fortran_program

    echo "$FUNCNAME" > dataset_kind.cfg
    echo "$nature_dir">nature_dir.cfg
    mkdir config; mv *.cfg config

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
#source   ${MODEL_DIR}/model_das_tools.sh

ens_assi_run $@
exit $?
