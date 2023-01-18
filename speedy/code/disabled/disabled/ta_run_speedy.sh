#!/bin/bash
#=======================================================================
# This script runs SPEEDY model with subdirectory $processor
#=======================================================================
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

# Reading external arguments
   speedy_dir=$1
  archive_dir=$2
analysis_time=$3
forecast_time=$4
       member=$5
    processor=$6
         
# Parsing external arguments
if [ $# -ne 6 ]
then
  echo "ta_run_speedy: Incorrect number of arguments"
  exit 1
fi

echo "Member $member running in processor $processor"

# Initialize processor folder
rm -rf $processor; mkdir $processor; cp imp.exe $processor; cd $processor

# link_input_units t30 
SB=${speedy_dir}/model/data/bc/t30/clim
SC=${speedy_dir}/model/data/bc/t30/anom

ln -s ${SB}/orog_lsm_alb.t30.grd         fort.20
ln -s ${SB}/sst_8190clim.t30.sea.grd     fort.21
ln -s ${SB}/seaice_8190clim.t30.sea.grd  fort.22
ln -s ${SB}/skt_8190clim.t30.land.grd    fort.23
ln -s ${SB}/sndep_8190clim.t30.land.grd  fort.24
ln -s ${SB}/veget.t30.land.grd           fort.25
ln -s ${SB}/soilw_8190clim.t30.land.grd  fort.26
cp    ${SC}/sst_anom_7990.t30.grd        fort.30

# Setting restart mode and initial time
ISTART=3 # Gridded time-average state and anomaly states (1 time step)
JSTART=2 # Gridded output restart file
echo $ISTART                      > fort.2
echo $JSTART                     >> fort.2

# Linking corresponding input restart file in sigma levels
# GRIDDED time average state
ln -fs ${archive_dir}/anal/${member}/${analysis_time}_TA.grd fort.92
# GRIDDED time average anomaly
ln -fs ${archive_dir}/guess/${member}/${analysis_time}_AN.grd fort.93

echo $analysis_time | cut -c1-4   > fort.7 # year
echo $analysis_time | cut -c5-6  >> fort.7 # month
echo $analysis_time | cut -c7-8  >> fort.7 # day
echo $analysis_time | cut -c9-10 >> fort.7 # hour
    
echo $forecast_time | cut -c1-4   > fort.8 # year
echo $forecast_time | cut -c5-6  >> fort.8 # month
echo $forecast_time | cut -c7-8  >> fort.8 # day
echo $forecast_time | cut -c9-10 >> fort.8 # hour

# Running speedy
./imp.exe > speedy.log # 2>&1

# Checking calendar consistency
forecast_time_ATGCM=$(cat fort.87)
if [ $forecast_time_ATGCM -ne $forecast_time ]; then
  echo "Calendar system conflict between bash and fortran components!!"
  exit 1
fi

# Storing Spectraly filtered analysis fields  
#mv ${analysis_time}_TA.grd ${archive_dir}/anal_f/${member}
#mv ${analysis_time}_AN.grd ${archive_dir}/anal_f/${member}

# Storing Background (forecast) fields
#mv state_ta_average.grd ${archive_dir}/guess/${member}/${forecast_time}.grd
#mv state_ta_anomaly.grd ${archive_dir}/guess/${member}/${forecast_time}_p.grd
mv state_ta_average.grd ${archive_dir}/guess/${member}/${forecast_time}_TA.grd
mv state_ta_anomaly.grd ${archive_dir}/guess/${member}/${forecast_time}_AN.grd

exit 0
