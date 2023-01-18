#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $CODE_DIR/running_scripts/common_tools.sh

create_station_set(){
    set_number=$1
    output_file=$2
    F90=gfortran

    cd "$(dirname "${BASH_SOURCE[0]}")"

    echo " Creating station set"
    case $set_number in
        1) empty_station_set;;
        2) homogeneous_station_set 0 ;;
        3) homogeneous_station_set 1 ;;
        4) homogeneous_station_set 2 ;;
        5) homogeneous_station_set 3 ;;
        6) homogeneous_station_set 4 ;;
        7) max_tree_network          ;;
        8) Breitenmosser_network     ;;
    esac
    echo " Station table $station_set was generated"

    # mv ${station_set}.tbl $output_file
    cat ${station_set}.tbl > $output_file

    # echo " Plotting station set with matlab"
    # command="matlab -r \"plot_station_set $station_set\""
    # eval $command
}

#=========================================================================
#> @brief Creates an empty station network to be used for free runs
#=========================================================================
empty_station_set(){
    funct_opening 2

    station_set="station_none"
    (cat<<_EOF_
  I  J
------
_EOF_
        )>${station_set}.tbl

    funct_closing 2
}

#=========================================================================
#> @brief Creates a homogeneous station network not covering the poles
#=========================================================================
homogeneous_station_set(){
    funct_opening 2

    gap=$1
    station_set="station_homogeneous_gap${gap}"

    lat_step=$(( gap + 1 ))
    lon_step=$(( gap + 1 ))

    cat > stations.f90 <<_EOF_
PROGRAM stations
IMPLICIT NONE
INTEGER,PARAMETER :: nlon=96
INTEGER,PARAMETER :: nlat=nlon/2
INTEGER,PARAMETER :: jj=3
INTEGER,PARAMETER :: lat_step=$lat_step
INTEGER,PARAMETER :: lon_step=$lon_step
INTEGER :: i,j

PRINT '(A)','  I  J'
PRINT '(A)','------'
DO j=1+jj, nlat-jj, lat_step
DO i=1, nlon, lon_step
PRINT '(2I3)',i,j
END DO
END DO
STOP
END PROGRAM stations
_EOF_

    $F90 stations.f90
    ./a.out > "${station_set}.tbl"

    rm -f stations.f90; rm -f a.out

    funct_closing 2
}

#=========================================================================
#> @brief Creates a station set with all gridboxes potentially
#> having trees. Criterion used: land-sea ratio >= 0.7 and albedo < 0.4
#=========================================================================
max_tree_network(){
    funct_opening 2

    station_set="station_tree_coverage_max"

    # Linking Surface geopotential height and orography_land-sea-mask_albedo
    ln -fs $CODE_DIR/speedy_tools/orography_t30.dat fort.21
    ln -fs $CODE_DIR/speedy_ver32/data/bc/t30/clim/orog_lsm_alb.t30.grd fort.20

    # Building and Running fortran code
    build_program.sh $CODE_DIR/obs/generate_max_tree_network.f90 $BIN_DIR > /dev/null
    generate_max_tree_network.exe > generate_max_tree_network.log

    mv max_tree_network.tbl ${station_set}.tbl
    rm -f generate_max_tree_network.exe
    # rm -f fort.21; rm -f fort.20

    funct_closing 2
}

#=========================================================================
#> @brief Creates a station set resembling Breitenmosser TRW network
#=========================================================================
Breitenmosser_network(){
    funct_opening 3

    max_tree_network

    station_set="Breitenmosser_network"
    generate_Breitenmosser_network.exe # > generate_Breitenmosser_network.log
#     rm -f generate_max_tree_network.exe
    funct_closing 3
}

create_station_set $@
exit $?

