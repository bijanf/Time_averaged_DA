#!/usr/bin/env bash
#=======================================================================
#> @brief Script to find an Spinned-up state of SPEEDY.
#> The model is started from rest and the final state is stored in
#> grid space representation
#=======================================================================
spinup_model(){
#    funct_opening always
    [[ $# -eq 4 ]] || error "Usage: spinup_model time_start time_length spinup_name storing_dir"
     time_start=$1
    time_length=$2
    spinup_name=$3
    storing_dir=$4
      
    echo "==============================================="
    echo " Performing a spinup run of SPEEDY model "
    echo "==============================================="
    echo " Setting enviroment"
    spinup_dir=${storing_dir}/${spinup_name}
    create_archive_dir.sh $spinup_dir || return 0
    
    cd $spinup_dir
    cp $SPEEDY/model/tmp/at_gcm.exe .
    speedy_link_forcings.sh "t30" .
    
    echo " Setting Time interval"
    echo "-----------------------------------------------"
    echo " time_start = $time_start"
    echo " time_length= $time_length hours"
    echo "-----------------------------------------------"
    
    echo " Running SPEEDY"
    speedy_run.sh 0 2 $time_start $time_length $(pwd)
    time_final=$(cat time_forecast.dat)
    echo " time_final = $time_final"
    
    echo " Saving metainformation"
    echo $time_start  > time_start.dat
    echo $time_final  > time_final.dat
    echo $time_length > time_length.dat
#    echo "spinup"     > dataset_type.dat
   
    echo " Cleaning up"
    rm -f at_gcm.exe
    rm -f fort.*

    echo "-----------------------------------------------"
    echo " Gridded spinned up state saved in"
    echo " $spinup_dir"
    echo "==============================================="
    
#    funct_closing always
    return 0 #success
}    

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

# Usage example:
#$ spinup_model.sh 1980010100 1440 spinup_1year $(pwd)

# Current hardwiring
#spinup_model 1979010100 $((24*1)) spinup_1979_1day $ARCHIVE_DIR | tee $ARCHIVE_DIR/spinup_1979_1day.log
#spinup_model 1979010100 $((24*30)) spinup_1979_30day $ARCHIVE_DIR | tee $ARCHIVE_DIR/spinup_1979_30day.log
#spinup_model 1979010100  8640 spinup_1979_1year $ARCHIVE_DIR | tee $ARCHIVE_DIR/spinup_1979_1year.log
#spinup_model 1980010100 17280 spinup_1979_2year $ARCHIVE_DIR | tee $ARCHIVE_DIR/spinup_1979_2year.log

spinup_model $@
exit $?
