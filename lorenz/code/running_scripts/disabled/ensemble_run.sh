#!/usr/bin/env bash
#set -xv
ensemble_run(){
    funct_opening 3
    dataset_name=$1
    spinup_name=$2
    nature_name=$3

    dataset_dir="$ARCH_DIR/$dataset_name"
    spinup_dir="$ARCH_DIR/$spinup_name"
    nature_dir="$ARCH_DIR/$nature_name"

    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir

    fortran_program=ensemble_run
    cp $WKDIR/${fortran_program}.exe .

    ln -s $spinup_dir/init*.dat .
    ln -s $nature_dir/nature*.dat .

    if [ $debug == "on" ]; then
        ddd ${fortran_program}.exe
    else
        ./${fortran_program}.exe
    fi
    rm *.exe init*.dat

    echo "$FUNCNAME" > dataset_kind.cfg
    mkdir config; mv *.cfg config

    das_plot.sh $dataset_dir

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
source   ${MODEL_DIR}/model_das_tools.sh

ensemble_run $@
exit $?
