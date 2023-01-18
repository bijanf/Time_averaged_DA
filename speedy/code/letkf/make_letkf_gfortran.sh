#!/bin/bash
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

PGM=letkf.exe
F90=mpif90
OMP=
F90OPT='-fconvert=big-endian -O3 -ffree-line-length-none'
INLINE=""
BLAS=0 #0:no blas  1:using blas
COMMONDIR=../../common

link_common(){
    # function to link common sources
    ln -fs $COMMONDIR/SFMT.f90 ./
    ln -fs $COMMONDIR/common.f90 ./
    ln -fs $COMMONDIR/common_mpi.f90 ./
    ln -fs $COMMONDIR/common_mtx.f90 ./
    ln -fs $COMMONDIR/common_letkf.f90 ./
    ln -fs $COMMONDIR/netlib.f ./
    ln -fs $COMMONDIR/netlibblas.f ./
    
    ln -fs ../common/common_speedy.f90 ./
    ln -fs ../common/common_mpi_speedy.f90 ./
    ln -fs ../common/common_obs_speedy.f90 ./
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

unlink_common
link_common
rm -f *.mod
rm -f *.o
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
$F90 $OMP $F90OPT -c common_mpi.f90
$F90 $OMP $F90OPT $INLINE -c common_mtx.f90
$F90 $OMP $F90OPT $INLINE -c netlib2.f
$F90 $OMP $F90OPT -c common_letkf.f90
$F90 $OMP $F90OPT $INLINE -c common_speedy.f90
$F90 $OMP $F90OPT -c common_obs_speedy.f90
$F90 $OMP $F90OPT -c common_mpi_speedy.f90
$F90 $OMP $F90OPT -c letkf_obs.f90
$F90 $OMP $F90OPT -c letkf_tools.f90
$F90 $OMP $F90OPT -c letkf.f90
$F90 $OMP $F90OPT -o ${PGM} *.o $LBLAS

rm -f *.mod
rm -f *.o
rm -f netlib2.f
sh ulnkcommon.sh

echo "NORMAL END"
