#!/bin/bash
echo "======================================================="
echo " Initializing system                   "
echo "======================================================="
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

#-----------------------------------------
# - Sourcing DA system function libraries
#-----------------------------------------
. scripts/system_functions.sh
. scripts/compiling_functions.sh
. scripts/time_functions.sh
. scripts/ta_low_level_functions.sh
. scripts/ta_high_level_functions.sh
. scripts/ta_plotting_functions.sh

#. scripts/da_low_level_functions_1.sh
#. scripts/da_low_level_functions_2.sh
#. scripts/da_plotting_functions.sh
#. scripts/da_high_level_functions.sh

#-------------------------------------
# Identifying server
#-------------------------------------
machine=$(hostname)      
echo "Running in $machine"

#--------------------------------------
# - Initialize paths
#--------------------------------------
# Absolute paths (Machine dependent)
case "$machine" in
    "tux04")
        SCRATCH_DIR="/scratch/users/acevedo/scratch"
        ARCHIVE_DIR="/scratch/users/acevedo/speedyDA_ARCHIVE";;
    "a41")
        SCRATCH_DIR="/scratch/acevedo"
        ARCHIVE_DIR="/scratch/acevedo/speedyDA_arch";;
    *)
        echo "unknown machine"
        exit 1;;
esac
#--------------------------------------
# - Relative paths
           HERE=`pwd`;
#             cd ../ARCHIVE
#    ARCHIVE_DIR=`pwd`       
   scripts_dir=${HERE}/scripts
        SPEEDY=${HERE}/speedy
  OBS_CODE_DIR=${SPEEDY}/obs 
LETKF_CODE_DIR=${SPEEDY}/letkf

       TMP_DIR=${SCRATCH_DIR}/speedy_DA   # work directory
 LETKF_TMP_DIR=${TMP_DIR}/ensfcst
    
    SPINUP_DIR=${ARCHIVE_DIR}/SPINUP/$SPINUP_NAME
  EXP_ARCH_DIR=${ARCHIVE_DIR}/EXP/$exp_name # experiment archive

    NATURE_DIR=${EXP_ARCH_DIR}/NATURE_RUN
  OBS_ARCH_DIR=${EXP_ARCH_DIR}/OBS
  FREE_RUN_DIR=${EXP_ARCH_DIR}/FREE_RUN
 ASSIM_RUN_DIR=${EXP_ARCH_DIR}/ASSIM_RUN   
#--------------------------------------
#         LETKF=letkf.exe
         
