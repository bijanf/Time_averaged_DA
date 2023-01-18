set_boundary_conditions(){
    # This function creates symbolic links to forcing fields
    # in work_dir folder (Adapted from inpfiles.s)
    
    local      res=$1 # resolution (t21, t30, ..)
    local work_dir=$2
    
    SB=${SPEEDY}/model/data/bc/${res}/clim
    SC=${SPEEDY}/model/data/bc/${res}/anom
    
    ln -fs $SB/orog_lsm_alb.${res}.grd         ${work_dir}/fort.20
    ln -fs $SB/sst_8190clim.${res}.sea.grd     ${work_dir}/fort.21
    ln -fs $SB/seaice_8190clim.${res}.sea.grd  ${work_dir}/fort.22
    ln -fs $SB/skt_8190clim.${res}.land.grd    ${work_dir}/fort.23
    ln -fs $SB/sndep_8190clim.${res}.land.grd  ${work_dir}/fort.24
    ln -fs $SB/veget.${res}.land.grd           ${work_dir}/fort.25
    ln -fs $SB/soilw_8190clim.${res}.land.grd  ${work_dir}/fort.26
    cp     $SC/sst_anom_7990.${res}.grd	      ${work_dir}/fort.30
    #cp    $SC/sst_anom_1950_1999_mean8190.${res}.grd  ${work_dir}/fort.30
    #cp    $SC/sst_anom_8190.${res}.sea.grd	         ${work_dir}/fort.30
    #cp    $SC/elnino_anom_p1.${res}.grd	             ${work_dir}/fort.30
    #cp    $HOME/speedy/hflux/clim_hflux_582.grd	 ${work_dir}/fort.31
}

run_speedy(){

	local       ISTART=$1 # Input  restart mode (info in com_tsteps.h)
	local       JSTART=$2 # Output restart mode (info in com_tsteps.h)
    local PRESENT_TIME=$3
    local   RUN_LENGTH=$4   
    
    FORECAST_TIME=`time_increment $PRESENT_TIME $RUN_LENGTH`

    echo ">>> BEGIN COMPUTATION AT $PRESENT_TIME"

    echo $ISTART                      > fort.2
    echo $JSTART                     >> fort.2
    
    echo $PRESENT_TIME  | cut -c1-4   > fort.7 # year
    echo $PRESENT_TIME  | cut -c5-6  >> fort.7 # month
    echo $PRESENT_TIME  | cut -c7-8  >> fort.7 # day
    echo $PRESENT_TIME  | cut -c9-10 >> fort.7 # hour
    
    echo $FORECAST_TIME | cut -c1-4   > fort.8 # year
    echo $FORECAST_TIME | cut -c5-6  >> fort.8 # month
    echo $FORECAST_TIME | cut -c7-8  >> fort.8 # day
    echo $FORECAST_TIME | cut -c9-10 >> fort.8 # hour
    
    # link in the current folder fortran units to input files
    local res=t30 # resolution (t21, t30, ..)
    SB=${SPEEDY}/model/data/bc/$res/clim
    SC=${SPEEDY}/model/data/bc/${res}/anom
    # ln -s or -fs within this function???
    ln -fs ${SB}/orog_lsm_alb.${res}.grd         fort.20
    ln -fs ${SB}/sst_8190clim.${res}.sea.grd     fort.21
    ln -fs ${SB}/seaice_8190clim.${res}.sea.grd  fort.22
    ln -fs ${SB}/skt_8190clim.${res}.land.grd    fort.23
    ln -fs ${SB}/sndep_8190clim.${res}.land.grd  fort.24
    ln -fs ${SB}/veget.${res}.land.grd           fort.25
    ln -fs ${SB}/soilw_8190clim.${res}.land.grd  fort.26
    cp     ${SC}/sst_anom_7990.${res}.grd	      fort.30
    #cp    $SC/sst_anom_1950_1999_mean8190.${res}.grd  fort.30
    #cp    $SC/sst_anom_8190.${res}.sea.grd	         fort.30
    #cp    $SC/elnino_anom_p1.${res}.grd	             fort.30
    #cp    $HOME/speedy/hflux/clim_hflux_582.grd	 fort.31

    # Run speedy 
    if [ $verbose -ge 1 ]; then
         ./imp.exe
    else
         ./imp.exe > /dev/null
    fi
    
    # Checking calendar consistency
    FORECAST_TIME_ATGCM=$(cat fort.87 | tee FORECAST_TIME.txt)
    if [ $FORECAST_TIME_ATGCM -ne $FORECAST_TIME ]; then
       echo "Calendar system conflict between bash and fortran components!!"
       exit 1
    fi

 	echo ">>> COMPUTATION ENDED AT $FORECAST_TIME"
}
