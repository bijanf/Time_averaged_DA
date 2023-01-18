#!/usr/bin/env bash
echo "======================================================="
echo "                 SPEEDY-TA-DA SYSTEM                   "
echo "======================================================="
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
#============================================================
task=$1
#============================================================
#      SYSTEM SETTINGS
#============================================================
        NODES=4              # Number of processors
      verbose=1              # Verbosity level (0,1,2)
#============================================================
#      Spinup DATA
#============================================================
#  SPINUP_NAME=spinup_10years
# SPINUP_START=1979010100
#SPINUP_LENGHT=0010000000

#  SPINUP_NAME=spinup_1year
# SPINUP_START=1979010100
#SPINUP_LENGHT=0001000000

  SPINUP_NAME=spinup_1month
 SPINUP_START=1979010100
SPINUP_LENGHT=0000010000

#============================================================
# Observational DATA
#============================================================
obs_network="max_tree_network.tbl"
#obs_network="station_1.tbl"
#============================================================
#      Experiment DATA
#============================================================
#     exp_name=ta_dae_new  # Name
#nature_length=0000000600  # must be at least 10 times assim_period due
#                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000006

#     exp_name=ta_dae_12months_ta18hr_assimp18hr  # Name
#nature_length=0001000000  # must be at least 10 times assim_period due
#                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000018

#     exp_name=ta_dae_12months_ta18hr_assimp18hr  # Name
#nature_length=0001000000  # must be at least 10 times assim_period due
#                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000018

#     exp_name=ta_dae_6months_ta12hr_assimp12hr  # Name
#nature_length=0000060000  # must be at least 10 times assim_period due
#                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000012

#     exp_name=ta_dae_3months_ta6hr_assimp6hr  # Name
#nature_length=0000030000  # must be at least 10 times assim_period due
#                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000006

#     exp_name=ta_dae_1month_ta6hr_assimp6hr  # Name
#nature_length=0000010000  # must be at least 10 times assim_period due
                          # to present initial ensemble generation
#   exp_length=$nature_length
# assim_period=0000000006

     exp_name=ta_dae_1week_ta6hr_assimp6hr  # Name
nature_length=0000000700  # must be at least 10 times assim_period due
                          # to present initial ensemble generation
   exp_length=$nature_length
 assim_period=0000000006

#=============================================================
      members=20          # Number of ensemble members
    NOSAVEENS=1  # Save flag. 0:all members 1: only mean/sprd
    tux04_compiler="gfortran"
    #tux04_compiler="ifort" # mpi compilation having problems
#=============================================================
echo "======================================================="
echo " Initializing system                   "
echo "======================================================="
. scripts/load_system_libraries.sh
. scripts/set_system_paths.sh

#---------------------------------------------------
#      TASKS LIST
#---------------------------------------------------
#--------------------------------------
# - Setting temporary work space
#--------------------------------------
#if [ -d "$TMP_DIR" ]; then
#   rm -rf $TMP_DIR   # remove preexisting work directory
#fi
case "$task" in
    "clean")
        rm -r $TMP_DIR # cleaning up
        ;;
    "init")
        [ -d $TMP_DIR ] || mkdir $TMP_DIR # create work directory
        compile_fortran_code
        ;;
    "run")
        #spinup_model $SPINUP_START $SPINUP_LENGHT
        #create_station_table
        ta_da_experiment
        ;;
    *)
        error_exit "unknown task";;
esac

#    ta_create_nature_run $nature_length # Start from the end of spinup
#    ta_create_observations $obs_network # During the whole nature run length
#    ta_assimilation_run $exp_length        "free" $FREE_RUN_DIR
#    ta_assimilation_run $exp_length "constrained" $ASSIM_RUN_DIR

#ta_plot_error $FREE_RUN_DIR anal
#ta_plot_error $ASSIM_RUN_DIR anal
#ta_plot_spread $ASSIM_RUN_DIR anal

#--------------------------------------------------------
wait # Don't clean up before all child processes finished

exit
