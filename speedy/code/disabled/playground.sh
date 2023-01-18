#!/usr/bin/env bash
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

verbose=1
  SPINUP_NAME=spinup_1month
 SPINUP_START=1979010100
SPINUP_LENGHT=0000010000
        NODES=4              # Number of processors
      members=20          # Number of ensemble members
    NOSAVEENS=1  # Save flag. 0:all members 1: only mean/sprd
    tux04_compiler="gfortran"

#obs_network="max_tree_network.tbl"
obs_network="station_1.tbl"

     exp_name=ta_dae_1month_ta6hr_assimp6hr  # Name
nature_length=0000010000  # must be at least 10 times assim_period due
                          # to present initial ensemble generation
   exp_length=$nature_length
 assim_period=0000000006

echo "======================================================="
echo " Initializing system                   "
echo "======================================================="
. scripts/load_system_libraries.sh
. scripts/set_system_paths.sh

#build_calculate_variance

#ta_create_observations $obs_network # During the whole nature run length

#. scripts/set_system_paths.sh
#           HERE=`pwd`;
#             cd ../ARCHIVE
#    ARCHIVE_DIR=`pwd`       
#   scripts_dir=${HERE}/scripts
#        SPEEDY=${HERE}/speedy
#  OBS_CODE_DIR=${SPEEDY}/obs 
#LETKF_CODE_DIR=${SPEEDY}/letkf


#ncview_speedy ${SPEEDY}/model/data/bc/t30/clim/orog_lsm_alb.t30.ctl
#speedy2nc orog_lsm_alb.t30.ctl
exit
