build_speedy(){
    funct_opening
    cd $TMP_DIR
    #=============================================
    # Build SPEEDY model in current directory
    #=============================================
    
    # Select apropriate makefile (Machine dependent)
    case "$machine" in
    "tux04")
        echo "Compiling with $tux04_compiler"
        
        case "$tux04_compiler" in
        "gfortran")
            cp $SPEEDY/model/update/makefile_gfortran $SPEEDY/model/update/makefile
            ;;
        "ifort")
            cp $SPEEDY/model/update/makefile_ifort $SPEEDY/model/update/makefile
            ;;
        *)
            echo "Unknown compiler $tux04_compiler"; exit 1
            ;;
        esac
        ;;
    "a41")
        cp $SPEEDY/model/update/makefile_xlf $SPEEDY/model/update/makefile
        ;;
    *)
        error_exit "unknown machine $machine"
        ;;
    esac

    # gather_code in current folder
    cp $SPEEDY/model/source/makefile .
    cp $SPEEDY/model/source/*.h .
    cp $SPEEDY/model/source/*.f .
    cp $SPEEDY/model/source/*.s .
    
    mv par_horres_t30.h atparam.h
    mv par_verres.h atparam1.h
    
    cp $SPEEDY/model/ver32.input/cls_*.h .
#    cp $SPEEDY/model/ver32.input/inpfiles.s .
    
    cp $SPEEDY/model/update/*.h .
    cp $SPEEDY/model/update/*.f .
    cp $SPEEDY/model/update/makefile .
    
    rm -f *.o; rm -f imp.exe;      # cleaning up
    
    # abacus stuff
    #. /opt/freeware/Modules/3.2.5/init/bash
    #module load gcc/pware


    # create executable
    if   [ $verbose -ge 2 ]; then
       make imp.exe
    elif [ $verbose -ge 1 ]; then
       make imp.exe > /dev/null
    else
       make imp.exe > /dev/null 2>&1
    fi
    
    funct_closing
}

build_program(){
    PGM=$1
    F90=$2 
    OMP=""
    F90OPT=$3
    INLINE=""
    F90LIB=""

    sh ulnkcommon.sh
    sh lnkcommon.sh
    rm -f *.mod
    rm -f *.o

    $F90 $OMP $F90OPT $INLINE -c SFMT.f90
    $F90 $OMP $F90OPT $INLINE -c common.f90
    $F90 $OMP $F90OPT $INLINE -c common_speedy.f90
    $F90 $OMP $F90OPT -c common_obs_speedy.f90
    $F90 $OMP $F90OPT -c ${PGM}.f90
    $F90 $OMP $F90OPT -o ${PGM}.exe *.o $F90LIB

    rm -f *.mod
    rm -f *.o
    sh ulnkcommon.sh

    echo "$PGM program created succesfully"
}


compile_fortran_code(){
    funct_opening always

    build_calculate_variance
    build_obs_program
    build_speedy
    build_LETKF
    
    funct_closing always
}

build_obs_program(){
    funct_opening
    cd ${OBS_CODE_DIR}
    
    case "$machine" in
    "tux04")
        case "$tux04_compiler" in
        "gfortran")
            bash make_generate_obs.sh "gfortran" "-O3 -fconvert=big-endian"
            ;;
        "ifort")
            bash make_generate_obs.sh "ifort" "-fast -O3 -convert big_endian"
            ;;
        *)
            echo "Unknown compiler $tux04_compiler"
            exit 1;;
        esac
        ;;
    "a41")
        bash make_generate_obs.sh "xlf"      "-O2"
        ;;
    *)
        error_exit "unknown machine";;
    esac
	#cp ${OBS_CODE_DIR}/* .  # bring obs. code to tmpdir
           # Select apropriate makefile (Machine dependent)
           # cp make_obsmake_gfortran.sh make_obsmake.sh
           # bash    make_obsmake.sh

#    if   [ $verbose -ge 2 ]; then
#        echo $'\n'"Building observation program"$'\n'
#        bash -x make_obsmake.sh
#    elif [ $verbose -ge 1 ]; then
#        bash    make_obsmake.sh
#    else
#        bash    make_obsmake.sh > /dev/null
#    fi
    funct_closing
}

build_LETKF(){
    funct_opening

    old_path=`pwd`
    cd $LETKF_CODE_DIR

    # Select apropriate makefile (Machine dependent)
    case "$machine" in
    "tux04")
        case "$tux04_compiler" in
        "gfortran")
            cp make_letkf_gfortran.sh make_letkf.sh
            ;;
        "ifort")
            cp make_letkf_ifort.sh make_letkf.sh
            ;;
        *)
            echo "Unknown compiler $tux04_compiler"
            exit 1;;
        esac

#        cp make_letkf_ifort.sh make_letkf.sh
#            # Load appropriate MPI module (Machine dependent)
#			module load mpich2/ifort-12.0.2/1.3.1
            # Load appropriate MPI module (Machine dependent)
#			module load mpich2/gfortran-4.4.5/1.3.1
        ;;
    "a41")
        cp make_letkf_xlf.sh make_letkf.sh
        ;;
    *)
        error_exit "unknown machine $machine";;
    esac

    # Create executable
    if   [ $verbose -ge 2 ]; then
       bash -x make_letkf.sh
    elif [ $verbose -ge 1 ]; then
       bash    make_letkf.sh
    else
       bash    make_letkf.sh > /dev/null
    fi

    cd $old_path
    
    funct_closing
}

