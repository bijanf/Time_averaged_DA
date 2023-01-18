#!/usr/bin/env bash
#set -xv
nature_obs(){
    funct_opening 3
    nature_dir=$1
    OUTDIR=$2

#   OBSNAME=regular13

    create_archive_dir $OUTDIR || return 0
    cd $OUTDIR

    fortran_program="$FUNCNAME"
    cp $WKDIR/${fortran_program}.exe .

    #ln -s $nature_dir/nature.dat .
    #ln -s $nature_dir/nature_Tvar.dat .
    ln -s $nature_dir/nature*.dat .
#   set_par_file ndays
#   set_par_file Taver_steps

    ./${fortran_program}.exe

    export_4byte_binaries_as_netcdf
#   cdo -f nc import_binary nature_clean_obs_Tvar_4byte.ctl nature_clean_obs_Tvar_4byte.nc
    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
source   ${MODEL_DIR}/model_das_tools.sh

nature_obs $@
exit $?
