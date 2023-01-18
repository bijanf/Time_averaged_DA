#!/usr/bin/env bash
model_list=("L63" "L63_2s" "L96_1s_pf" "L96_2s" "L96_2s_mix")
export model=${model:-${model_list[1]}}

echo " - Folder structure"

export  CODE_DIR=$(pwd)
export   BIN_DIR=$CODE_DIR/../bin
export   COM_DIR=$CODE_DIR/common
export   DAS_DIR=$CODE_DIR/running_scripts
#export MODEL_DIR=$CODE_DIR/models/$model

MODEL_DIR=$CODE_DIR/models/$model

if [[ -n ${DAS_INITIALIZED:-} ]];then
    remove_from_PATH $BIN_DIR
    remove_from_PATH $DAS_DIR
fi
export PATH=$BIN_DIR:$DAS_DIR:$PATH


export ARCH_THERE="/scratch/acevedo/lorenz-das_copies"

TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-lorenz_copy${count}"; }
