#!/usr/bin/env bash
#=========================================================================
#> @brief Compile all fortran programs for a given model
#=========================================================================
das_make_all(){
    funct_opening 3

    model_name=$1
    echo " Compiling programs for model $model_name"

    configure

    export  MODEL_DIR=$CODE_DIR/$model_name
    cd $DAS_DIR; rm -rf bin; mkdir -p bin; cd bin

    programs=( \
        "$COM_DAS_DIR/ensemble_assi_run.f90" \
        "$COM_DAS_DIR/sampling_run.f90" \
        "$COM_DAS_DIR/nature_obs.f90" \
        "$COM_DAS_DIR/nature_run.f90" \
        "$COM_DAS_DIR/ensemble_free_run.f90" \
        )

    for program_path in ${programs[@]}; do
        build_program "$program_path"
    done
    funct_closing 3
}

#=========================================================================
#> @brief Set machine dependent compiling variables
#=========================================================================
configure(){
    funct_opening 2
    echo " Finding out right configuration"

    # - Identifying server
    echo " - Running in $(hostname)"

    # - Configuring compilers
    case "$F90" in
        gfortran) gfortran_config ;;
        ifort   )    ifort_config ;;
        *       ) error "Unsupported compiler $F90";;
    esac

    funct_closing 2
}

gfortran_config(){
    F90_opt="-ffree-line-length-none -lblas -Wall -Wextra -ffpe-trap=invalid,zero,overflow" #,undeflow
    case $DBG in
        "on" ) DBG="-g3 -fbacktrace -fbounds-check"
            echo " - Compiling with debugging flags" ;;
        "off") DBG="-O3"
            echo " - Compiling with optimization"    ;;
        *    ) error "Unknown debugging flag $DBG"   ;;
    esac

    case "$(hostname)" in
        negrito)            ;;
        tux04)            ;;
        calc01|calc02)
            F90=gfortran-4.3
            ;;
        calc03)
            F90=gfortran-4.7
            ;;
        soroban|node???)
            module load blas/gcc/64/1
            ;;
                                #"a41") module load mpich2/gfortran-4.4.5/1.3.1;;
        * ) echo " unknown machine $(hostname)"; return 1
            ;;
    esac
}

ifort_config(){
    
    F90_opt="-lblas -fpe=0 -fpe-all=0 -extend-source -warn all"
    case $DBG in
        "on" )
            DBG="-O0 -g -traceback -check"
#           DBG="-O0 -debug all -traceback -check bounds"
            echo " - Compiling with debugging flags" ;;
        "off")
	    DBG="-O3"
            echo " - Compiling with optimization"    ;;
        *    )
	    error "Unknown debugging flag $DBG"      ;;
    esac

    case "$(hostname)" in
        negrito)            ;;
        tux04|calc02)        ;;
        calc03)
            source /net/opt/system/modules/default/init/bash
            module load ifort/12.0.2
            ;;
        #"a41") module load mpich2/gfortran-4.4.5/1.3.1;;
        * ) echo " unknown machine $(hostname)"; return 1
            ;;
    esac
}

#=========================================================================
#> @brief Build a fortran executable
#=========================================================================
build_program(){
    funct_opening 2

    [[ $# -eq 1 ]] || error "Usage: build_program program_code_fullpath"
    program_code_fullpath=$1

    program_name=$(basename "$program_code_fullpath")
    program_name="${program_name%.*}"
    #extension="${filename##*.}"
    program_dir=$(dirname ${program_code_fullpath})

    print_line 1
    echo " - Program $program_name"
    print_line 1

    files=( \
	"$COM_DIR/SFMT.f90" \
        "$COM_DIR/common.f90" \
        "$COM_DIR/netlib.f" \
        "$COM_DIR/common_mtx.f90" \
        "$COM_DAS_DIR/common_das_tools.f90" \
        "$MODEL_DIR/model_core.f90" \
        "$COM_DAS_DIR/common_dyn_system.f90" \
        "$COM_DAS_DIR/io_tools.f90" \
        "$COM_DAS_DIR/common_obs_operator.f90" \
        "$COM_DAS_DIR/ensemble_tools.f90" \
        "$COM_DIR/common_letkf.f90" \
        "$COM_DAS_DIR/common_trajectory.f90" \
        "$COM_DAS_DIR/common_filter.f90" \
        "$program_dir/${program_name}.f90" \
	)

    $F90 $F90_opt $DBG -o ${program_name}.exe ${files[@]}
# #    cat $COM_DIR/netlibblas.f >> ./netlib.f

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
das_make_all $@
exit $?

