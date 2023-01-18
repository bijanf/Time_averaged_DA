#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source $CODE_DIR/running_scripts/common_tools.sh

#=========================================================================
#> @brief Build programs which use Miyoshi's common F90 modules
#>
#> Usage example:
#> $ build_program.sh generate_observations
#=========================================================================
build_program(){
    # [[ $# -eq 1 ]] || error "Usage: build_program program"
    program_path=$1
    target_dir=${2:-"."}
    program_name="$(barename $program_path)"
    # DBG=on
    DBG=${debug:-on}
#     DBG=off
    COMMON=$CODE_DIR/common
    COMMON_SPEEDY=$CODE_DIR/speedy_tools

    cd $target_dir

    echo " Finding out right configuration" # (machine dependent)
    configure

    unlink_common
    link_common
    rm -f *.mod *.o

    F90_OPT="$FFLAGS $DBG_FLAGS"

    $F90 $OMP $F90_OPT $INLINE -c SFMT.f90
    $F90 $OMP $F90_OPT $INLINE -c common.f90
    $F90 $OMP $F90_OPT $INLINE -c common_speedy.f90
    $F90 $OMP $F90_OPT $INLINE -c common_tools.f90
    $F90 $OMP $F90_OPT -c common_obs_speedy.f90
    $F90 $OMP $F90_OPT -c ${program_path}
    $F90 $OMP $F90_OPT -o ${program_name}.exe *.o $F90LIB


    rm -f *.mod *.o
    unlink_common

    echo " $program_name program created succesfully"
}

configure(){
    OMP=""
    INLINE=""
    F90LIB=""
            case "$F90" in
                "gfortran")
                    gfortran_config ;;
                    # F90_OPT="-O3 -fconvert=big-endian";;
                "ifort")
                    F90_OPT="-fast -O3 -convert big_endian";;
                *)
                    error "Unknown compiler $F90"
                    exit 1;;
            esac
    #case "$(hostname)" in
        #"tux04")
            #tux04_F90="gfortran"
            #F90=$tux04_F90
            #case "$F90" in
                #"gfortran")
                    #gfortran_config ;;
                    ## F90_OPT="-O3 -fconvert=big-endian";;
                #"ifort")
                    #F90_OPT="-fast -O3 -convert big_endian";;
                #*)
                    #error "Unknown compiler $F90"
                    #exit 1;;
            #esac
            #;;
        #*)
            #error "unknown machine";;
    #esac

}

gfortran_config(){
    FFLAGS="-fconvert=big-endian -ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow
    # FFLAGS="-ffree-line-length-none -lblas -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow
    case $DBG in
        "on" ) DBG_FLAGS="-g3 -fbacktrace -fbounds-check"
            echo " - Compiling with debugging flags" ;;
        "off") DBG_FLAGS="-O3"
            echo " - Compiling with optimization"    ;;
        *    ) error "Unknown debugging status $DBG"   ;;
    esac

    # case "$(hostname)" in
    #     negrito)                           ;;
    #     tux04 |tux21|calc02|calc03|calc04) ;;
    #     soroban|node???)
    #     module load blas/gcc/64/1          ;;
    #     * ) error " unknown machine $(hostname)";;
    # esac
}

#=======================================================================
#> @brief Create soft links to the common sources
#=======================================================================
link_common(){

    ln -fs $COMMON/SFMT.f90 ./
    ln -fs $COMMON/common.f90 ./
    ln -fs $COMMON/common_mpi.f90 ./
    ln -fs $COMMON/common_obs.f90 ./
    ln -fs $COMMON/common_mtx.f90 ./
    ln -fs $COMMON/common_letkf.f90 ./
    ln -fs $COMMON/netlib.f ./

    cp $COMMON/common_tools.f90 ./

    ln -fs $COMMON_SPEEDY/common_speedy.f90 ./
    ln -fs $COMMON_SPEEDY/common_mpi_speedy.f90 ./
    ln -fs $COMMON_SPEEDY/common_obs_speedy.f90 ./
}

#=======================================================================
#> @brief Delete links of common sources
#=======================================================================
unlink_common(){
    rm -f SFMT.f90
    rm -f common.f90
    rm -f common_mpi.f90
    rm -f common_obs.f90
    rm -f common_mtx.f90
    rm -f common_letkf.f90
    rm -f netlib.f

    rm -f common_tools.f90

    rm -f common_speedy.f90
    rm -f common_mpi_speedy.f90
    rm -f common_obs_speedy.f90
}

build_program $@
exit $?

