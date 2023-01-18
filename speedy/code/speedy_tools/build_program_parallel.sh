#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $CODE_DIR/running_scripts/common_tools.sh

build_program_par(){

    program_path=$1
    target_dir=${2:-"."}
    program_name="$(barename $program_path)"

    # DBG=on
    DBG=${debug:-on}

    #cd $(dirname "${BASH_SOURCE[0]}")
    cd $target_dir

    F90=mpif90
    OMP=

    case $DBG in
        "on" )
	  F90OPT='-fconvert=big-endian -g3 -fbacktrace -fbounds-check -ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow'
	  echo " - Compiling with debugging flags" ;;
        "off") 
	  F90OPT='-fconvert=big-endian -O3 -ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow'
            echo " - Compiling with optimization"    ;;
        *    ) error "Unknown debugging status $DBG"   ;;
    esac

#     # for debug
#     F90OPT='-fconvert=big-endian -g3 -fbacktrace -fbounds-check -ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow'
#     # optimized
#     #F90OPT='-fconvert=big-endian -O3 -ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow'
    INLINE=""
    BLAS=0 #0:no blas  1:using blas
    
    COMMON_DIR=$CODE_DIR/common
    SPEEDY_COM=$CODE_DIR/speedy_tools
    SPEEDY_LETKF=$CODE_DIR/letkf

    link_common
    rm -f *.mod *.o
    
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
    $F90 $OMP $F90OPT $INLINE -c common_tools.f90

    $F90 $OMP $F90OPT         -c common_mpi.f90
    $F90 $OMP $F90OPT $INLINE -c common_mtx.f90
    $F90 $OMP $F90OPT $INLINE -c netlib2.f
    $F90 $OMP $F90OPT         -c ensemble_size.f90
    $F90 $OMP $F90OPT         -c common_letkf.f90
    $F90 $OMP $F90OPT $INLINE -c common_speedy.f90


    $F90 $OMP $F90OPT         -c common_obs_speedy.f90
    $F90 $OMP $F90OPT         -c common_mpi_speedy.f90
    $F90 $OMP $F90OPT         -c letkf_obs.f90
    $F90 $OMP $F90OPT         -c letkf_tools.f90


    $F90 $OMP $F90OPT         -c ${program_path}
    $F90 $OMP $F90OPT -o ${program_name}.exe *.o $LBLAS

    rm *.mod *.o netlib2.f
    unlink_common

    echo "NORMAL END"
}

link_common(){
    # function to link common sources
    ln -fs $COMMON_DIR/SFMT.f90 .
    ln -fs $COMMON_DIR/common.f90 .
    ln -fs $COMMON_DIR/common_mpi.f90 .
    ln -fs $COMMON_DIR/common_mtx.f90 .
    ln -fs $COMMON_DIR/common_letkf.f90 .
    ln -fs $COMMON_DIR/ensemble_size.f90 .
    ln -fs $COMMON_DIR/netlib.f .
    ln -fs $COMMON_DIR/netlibblas.f .
    ln -fs $SPEEDY_COM/common_speedy.f90 .
    ln -fs $SPEEDY_COM/common_mpi_speedy.f90 .
    ln -fs $SPEEDY_COM/common_obs_speedy.f90 .
    ln -fs $SPEEDY_LETKF/letkf_obs.f90 .
    ln -fs $SPEEDY_LETKF/letkf_tools.f90 .

    cp $COMMON_DIR/common_tools.f90 ./
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
    
    rm -f ensemble_size.f90

    rm -f common_speedy.f90
    rm -f common_mpi_speedy.f90
    rm -f common_obs_speedy.f90
    rm -f letkf_obs.f90
    rm -f letkf_tools.f90

    rm -f common_tools.f90
}

build_program_par $@
exit $?

