#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#=======================================================================
#> @brief Script to build SPEEDY model executable
#=======================================================================
build_speedy()
{
    [ $# -eq 2 ] || error "Incorrect args number."
    RES=$1          # Spectral resolution
    F77=$2          # Compiler to be used

    # Locate main speedy dir
    SPEEDY="$(dirname "${BASH_SOURCE[0]}")"
    SA=$SPEEDY/model/source # Original source dir
    CA=$SPEEDY/model/tmp    # Temporary dir

    # Copy files from basic version directory

    echo "copying from $SA/source to $CA"
    rm -fr $CA
    mkdir $CA
    cd $CA
    #rm -f $CA/*

    cp $SA/makefile $CA/
    cp $SA/*.f      $CA/
    cp $SA/*.h      $CA/
#       cp $SA/*.s      $CA/

    mv $CA/par_horres_$1.h   $CA/atparam.h
    mv $CA/par_verres.h      $CA/atparam1.h

    # Copy parameter and namelist files from user's .input directory

    echo " "
    echo "ver32.input new files ..."
    ls $SPEEDY/model/ver32.input

    echo " "
    echo "copying parameter and namelist files from $SPEEDY/model/ver32.input "
    cp $SPEEDY/model/ver32.input/cls_*.h     $CA/
#       cp $SPEEDY/model/ver32.input/inpfiles.s  $CA/
#       cp $SPEEDY/model/ver32.input/cls_*.h     $SPEEDY/model/input/exp_$2
#       cp $SPEEDY/model/ver32.input/inpfiles.s  $SPEEDY/model/input/exp_$2

    # Copy modified model files from user's update directory

    echo " "
    echo "update new files ..."
    ls $SPEEDY/model/update

    echo " "
    echo "copying modified model files from $SPEEDY/model/update"
    cp $SPEEDY/model/update/* $CA/

    # Select apropriate makefile (Machine dependent)
    mv $CA/makefile_${F77} $CA/makefile

    # Create executable
    echo " "
    echo "compiling with $F77"
    make at_gcm.exe

    echo " "
    echo "Successful compilation"

    return 0 # Success

    #funct_opening
    #cd $TMP_DIR
    #=============================================
    # Build SPEEDY model in current directory
    #=============================================

    ## Select apropriate makefile (Machine dependent)
    #case "$machine" in
    #"tux04")
        #echo "Compiling with $tux04_compiler"

        #case "$tux04_compiler" in
        #"gfortran")
            #cp $SPEEDY/model/model/update/makefile_gfortran $SPEEDY/model/model/update/makefile
            #;;
        #"ifort")
            #cp $SPEEDY/model/model/update/makefile_ifort $SPEEDY/model/model/update/makefile
            #;;
        #*)
            #echo "Unknown compiler $tux04_compiler"; exit 1
            #;;
        #esac
        #;;
    #"a41")
        #cp $SPEEDY/model/model/update/makefile_xlf $SPEEDY/model/model/update/makefile
        #;;
    #*)
        #error "unknown machine $machine"
        #;;
    #esac

    ## gather_code in current folder
    #cp $SPEEDY/model/model/source/makefile .
    #cp $SPEEDY/model/model/source/*.h .
    #cp $SPEEDY/model/model/source/*.f .
    #cp $SPEEDY/model/model/source/*.s .

    #mv par_horres_t30.h atparam.h
    #mv par_verres.h atparam1.h

    #cp $SPEEDY/model/model/ver32.input/cls_*.h .
##    cp $SPEEDY/model/model/ver32.input/inpfiles.s .

    #cp $SPEEDY/model/model/update/*.h .
    #cp $SPEEDY/model/model/update/*.f .
    #cp $SPEEDY/model/model/update/makefile .

#    rm -f *.o; rm -f imp.exe;      # cleaning up

    # abacus stuff
    #. /opt/freeware/Modules/3.2.5/init/bash
    #module load gcc/pware


    ## create executable
    #if   [ $verbose -ge 2 ]; then
       #make imp.exe
    #elif [ $verbose -ge 1 ]; then
       #make imp.exe > /dev/null
    #else
       #make imp.exe > /dev/null 2>&1
    #fi

    #funct_closing
}

build_speedy t30 gfortran
exit $?
