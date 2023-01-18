#!/usr/bin/env bash
#set -xv
das_launch(){
    funct_opening 3

    fortran_program=$1;   echo $fortran_program;
    echo $2
    echo ${input_files[@]}
    echo $input_files
    input_array_name=$2[@]
    output_dir=$3
    input_array=("${!input_array_name}")
    echo "${input_array}[@]"
    return 1
    exit

    create_archive_dir $output_dir || return 0
    cd $output_dir

    cp $WKDIR/${fortran_program}.exe .


    echo ${${input_array_name}[@]}
    echo ${input_files[1]}
    exit


    ln -s $spinup_dir/init.dat .

    ./${fortran_program}.exe

    rm *.exe

    init.dat

  #save_metainfo(){
    echo "${fortran_program}" > dataset_kind.cfg
    mkdir config; mv *.cfg config

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source  ${COM_DIR}/common_das_tools.sh
source ${MODEL_DIR}/model_das_tools.sh

das_launch $@
exit $?
