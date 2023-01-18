#!/usr/bin/env bash
model_list=("L63" "L63_2s" "L96_1s_pf" "L96_2s" "L96_2s_mix")
export model
export model=${model:-${model_list[1]}}

export   F90=${F90:-ifort}
#export   F90=${F90:-gfortran}

echo " - Folder structure"

export  CODE_DIR=$(pwd)
export   BIN_DIR=$CODE_DIR/../bin
export   COM_DIR=$CODE_DIR/common
export   DAS_DIR=$CODE_DIR/running_scripts
#export MODEL_DIR=$CODE_DIR/models/$model

export MODEL_DIR=$CODE_DIR/models/$model

if [[ -n ${DAS_INITIALIZED:-} ]];then
    remove_from_PATH $BIN_DIR
    remove_from_PATH $DAS_DIR
fi
export PATH=$BIN_DIR:$DAS_DIR:$PATH


export ARCH_THERE="/scratch/acevedo/lorenz-das_copies"

TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-lorenz_copy${count}"; }

#=========================================================================
#> @brief Compile all fortran programs for a given model
#=========================================================================
das_make_all(){
    funct_opening 3

    model_name=$1

    echo " Building programs for model $model_name with $F90"

    configure

    echo " Copying sources to bin folder"

    MODEL_DIR=$CODE_DIR/models/$model_name
    rm -rf $BIN_DIR; mkdir -p $BIN_DIR; cd $BIN_DIR
    cp $COM_DIR/*.{f,f90} .
    cp $MODEL_DIR/model_core.f90 .
#    cat $COM_DIR/netlibblas.f >> ./netlib.f

    echo " Compiling modules"

    modules=( \
        SFMT.f90 \
        common.f90 \
        common_tools.f90 \
        ensemble_size.f90 \
        station_data.f90 \
        model_core.f90 \
        dynamical_system.f90 \
        io_tools.f90 \
        observation_operator.f90 \
        ensemble_tools.f90 \
        trajectory.f90 \
        filter.f90 \
        )

    for module in ${modules[@]}; do
        echo " - $module"
        $F90 $FFLAGS $DBG_FLAGS -c $module
    done

    echo " Linking programs"

    programs=( \
        sampling_run \
        nature_run \
        nature_obs \
        ensemble_free_run \
        ensemble_assi_run \
        )

    names=(${modules[@]%.*})
    for name in ${names[@]}; do
        objs=(${objs[@]:-} ${name}.o)
    done

    for program_name in ${programs[@]}; do
        echo " - $program_name"
        $F90 $FFLAGS $DBG_FLAGS -o ${program_name}.exe ${program_name}.f90 ${objs[@]}
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

    FFLAGS="-ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow
    # FFLAGS="-ffree-line-length-none -lblas -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow

    case $DBG in
        "on" )
            DBG_FLAGS="-g3 -fbacktrace -fbounds-check"
            echo " - Compiling with debugging flags" ;;
        "off")
            DBG_FLAGS="-O3"
            echo " - Compiling with optimization"    ;;
        *    )
            error "Unknown debugging status $DBG"   ;;
    esac

    case "$(hostname)" in
        negrito);;
        tux04 |tux21|calc01|calc02|calc03|calc04);;
        soroban|node???) module load blas/gcc/64/1          ;;
        *              ) error " unknown machine $(hostname)";;
    esac
    }

    ifort_config(){

        FFLAGS="-fpe=0 -fpe-all=0 -extend-source -warn"
    #FFLAGS="-lblas -fpe=0 -fpe-all=0 -extend-source -warn"
        case $DBG in
            "on" )
                DBG_FLAGS="-O0 -g -traceback -check"
#           DBG_FLAGS="-O0 -debug all -traceback -check bounds"
                echo " - Compiling with debugging flags" ;;
            "off")
                DBG_FLAGS="-O3"
                echo " - Compiling with optimization"    ;;
            *    )
                error "Unknown debugging status $DBG"    ;;
        esac

        case "$(hostname)" in
            negrito)            ;;
            tux04|calc02|tux21) ;;
            calc01|calc03|calc04)
                source /net/opt/system/modules/default/init/bash
                module load ifort/12.0.2
                ;;
            soroban|node???)
                module load intel/compiler/64/12.0/2011.4.191
                ;;
            * ) error " unknown machine $(hostname)";;
        esac
    }
