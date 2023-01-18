#!/bin/bash
#=======================================================================
# ensfcst.sh
#   This script runs the SPEEDY model with subdirectory $NODE
#=======================================================================
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

# Reading external arguments
       SPEEDY=$1
       OUTPUT=$2
ANALYSIS_TIME=$3
FORECAST_TIME=$4
       MEMBER=$5
         NODE=$6

# Parsing external arguments
if [ 5$6 -eq 5 ]; then
    echo "ERROR in ensfcst.sh"
	exit
fi

# Initialize NODE folder
rm -rf $NODE; mkdir $NODE
cp imp.exe $NODE
cd $NODE

#link_input_units t30 
SB=$SPEEDY/model/data/bc/t30/clim
SC=$SPEEDY/model/data/bc/t30/anom
ln -s $SB/orog_lsm_alb.t30.grd         fort.20
ln -s $SB/sst_8190clim.t30.sea.grd     fort.21
ln -s $SB/seaice_8190clim.t30.sea.grd  fort.22
ln -s $SB/skt_8190clim.t30.land.grd    fort.23
ln -s $SB/sndep_8190clim.t30.land.grd  fort.24
ln -s $SB/veget.t30.land.grd           fort.25
ln -s $SB/soilw_8190clim.t30.land.grd  fort.26
cp    $SC/sst_anom_7990.t30.grd        fort.30


# Linking corresponding GRIDDED restart file in sigma levels
ln -fs $OUTPUT/anal/$MEMBER/$ANALYSIS_TIME.grd fort.90 

# Setting restart mode and initial time
ISTART=2 # Gridded input  restart file
JSTART=2 # Gridded output restart file

echo $ISTART                      > fort.2
echo $JSTART                     >> fort.2
    
echo $ANALYSIS_TIME | cut -c1-4   > fort.7 # year
echo $ANALYSIS_TIME | cut -c5-6  >> fort.7 # month
echo $ANALYSIS_TIME | cut -c7-8  >> fort.7 # day
echo $ANALYSIS_TIME | cut -c9-10 >> fort.7 # hour
    
echo $FORECAST_TIME | cut -c1-4   > fort.8 # year
echo $FORECAST_TIME | cut -c5-6  >> fort.8 # month
echo $FORECAST_TIME | cut -c7-8  >> fort.8 # day
echo $FORECAST_TIME | cut -c9-10 >> fort.8 # hour

# Running speedy
./imp.exe > imp.log 2>&1

# Checking calendar consistency
FORECAST_TIME_ATGCM=$(cat fort.87)
if [ $FORECAST_TIME_ATGCM -ne $FORECAST_TIME ]; then
  echo "Calendar system conflict between bash and fortran components!!"
  exit 1
fi

# Archiving Analysis and Forecast output fields
mv ${ANALYSIS_TIME}.grd   $OUTPUT/anal_f/$MEMBER
mv ${ANALYSIS_TIME}_p.grd $OUTPUT/anal_f/$MEMBER
mv ${FORECAST_TIME}.grd   $OUTPUT/gues/$MEMBER
mv ${FORECAST_TIME}_p.grd $OUTPUT/gues/$MEMBER
exit 0
