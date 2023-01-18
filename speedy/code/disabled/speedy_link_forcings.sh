#!/usr/bin/env bash
#=======================================================================
#> @brief This function creates symbolic links to forcing fields
#> in work_dir folder (Adapted from inpfiles.s)
#=======================================================================
speedy_link_forcings()
{
	[[ $# -eq 2 ]] || error "Usage: speedy_link_forcings res work_dir"
         res=$1 # resolution (t21, t30, ..)
    work_dir=$2

	[[ $res == t30 ]] || error "Unsuported resolution $res"
	
    SB=${SPEEDY}/model/data/bc/${res}/clim
    SC=${SPEEDY}/model/data/bc/${res}/anom
    
    ln -fs        $SB/orog_lsm_alb.${res}.grd  ${work_dir}/fort.20
    ln -fs    $SB/sst_8190clim.${res}.sea.grd  ${work_dir}/fort.21
    ln -fs $SB/seaice_8190clim.${res}.sea.grd  ${work_dir}/fort.22
    ln -fs   $SB/skt_8190clim.${res}.land.grd  ${work_dir}/fort.23
    ln -fs $SB/sndep_8190clim.${res}.land.grd  ${work_dir}/fort.24
    ln -fs          $SB/veget.${res}.land.grd  ${work_dir}/fort.25
    ln -fs $SB/soilw_8190clim.${res}.land.grd  ${work_dir}/fort.26
    cp           $SC/sst_anom_7990.${res}.grd  ${work_dir}/fort.30
    #cp    $SC/sst_anom_1950_1999_mean8190.${res}.grd  ${work_dir}/fort.30
    #cp    $SC/sst_anom_8190.${res}.sea.grd	         ${work_dir}/fort.30
    #cp    $SC/elnino_anom_p1.${res}.grd	             ${work_dir}/fort.30
    #cp    $HOME/speedy/hflux/clim_hflux_582.grd	 ${work_dir}/fort.31
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

speedy_link_forcings $@
exit $?
