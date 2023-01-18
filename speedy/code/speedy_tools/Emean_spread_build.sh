#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
cd $(dirname "${BASH_SOURCE[0]}")

PGM=Emean_spread
F90=mpif90
OMP=
F90OPT='-fconvert=big-endian -O3 -ffree-line-length-none'
INLINE=""
BLAS=0 #0:no blas  1:using blas
COMMON_DIR=$CODE_DIR/common
SPEEDY_COM=$CODE_DIR/speedy/common
SPEEDY_LETKF=$CODE_DIR/speedy/letkf

link_common(){
    # function to link common sources
    ln -fs $COMMON_DIR/SFMT.f90 .
    ln -fs $COMMON_DIR/common.f90 .
    ln -fs $COMMON_DIR/common_mpi.f90 .
    ln -fs $COMMON_DIR/common_mtx.f90 .
    ln -fs $COMMON_DIR/common_letkf.f90 .
    ln -fs $COMMON_DIR/netlib.f .
    ln -fs $COMMON_DIR/netlibblas.f .
    ln -fs $SPEEDY_COM/common_speedy.f90 .
    ln -fs $SPEEDY_COM/common_mpi_speedy.f90 .
    ln -fs $SPEEDY_COM/common_obs_speedy.f90 .
    ln -fs $SPEEDY_LETKF/letkf_obs.f90 .
    ln -fs $SPEEDY_LETKF/letkf_tools.f90 .
    
}

unlink_common(){
    # function to delete links to common sources    
    rm -f SFMT.f90
    rm -f common.f90
    rm -f common_mpi.f90
    rm -f common_mtx.f90
    rm -f common_letkf.f90
    rm -f netlib.f
    rm -f netlibblas.f
    
    rm -f common_speedy.f90
    rm -f common_mpi_speedy.f90
    rm -f common_obs_speedy.f90
}

link_common
cat netlib.f > netlib2.f

if test $BLAS -eq 1
then
    LBLAS="-L/usr/lib -lblas"
else
    cat netlibblas.f >> netlib2.f
    LBLAS=""
fi

$F90 $OMP $F90OPT $INLINE -c SFMT.f90
$F90 $OMP $F90OPT $INLINE -c common.f90
$F90 $OMP $F90OPT         -c common_mpi.f90
$F90 $OMP $F90OPT $INLINE -c common_mtx.f90
$F90 $OMP $F90OPT $INLINE -c netlib2.f
$F90 $OMP $F90OPT         -c common_letkf.f90
$F90 $OMP $F90OPT $INLINE -c common_speedy.f90
$F90 $OMP $F90OPT         -c common_obs_speedy.f90
$F90 $OMP $F90OPT         -c common_mpi_speedy.f90
$F90 $OMP $F90OPT         -c letkf_obs.f90
$F90 $OMP $F90OPT         -c letkf_tools.f90
$F90 $OMP $F90OPT         -c ${PGM}.f90
$F90 $OMP $F90OPT -o ${PGM}.exe *.o $LBLAS

rm *.mod *.o netlib2.f
unlink_common

echo "NORMAL END"
