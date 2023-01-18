#!/bin/bash
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

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
    ARCHIVE_DIR="/scratch/users/acevedo/speedyDA_ARCHIVE"
    setup_modules_system
    case "$tux04_compiler" in
    "gfortran")
        module load mpich2/gfortran-4.4.5/1.3.1
        ;;
    "ifort")
        module load mpich2/ifort-12.0.2/1.3.1
        ;;
    *)
        echo "Unknown compiler $tux04_compiler"
        exit 1;;
    esac
    ;;
"a41")
    SCRATCH_DIR="/scratch/acevedo"
    ARCHIVE_DIR="/scratch/acevedo/speedyDA_arch"
    ;;
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
         
