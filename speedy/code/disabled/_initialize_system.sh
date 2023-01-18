#!/usr/bin/env bash
set -o nounset # Unset variable expansion gives error
cd "$(dirname "${BASH_SOURCE[0]}")" # Go to folder where this file lies
source tools/common_tools.sh

das_initialize(){
    echo "======================================"
    echo " Initializing DAS enviroment"
    echo "======================================"

    model=speedy

    echo " - Setting environment"
    export      DAS="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    SPEEDY_BIN="$DAS/speedy/model/tmp"
    export SPEEDY="$DAS/speedy"
    export  LETKF="${DAS}/speedy/letkf"
    export  TOOLS="${DAS}/tools"


    # echo " - Folder structure"
    # export STATIONS="${DAS}"/stations
    # export      OBS="${DAS}"/obs
    # export PARTICLE="${DAS}"/particle
    # export ENSEMBLE="${DAS}"/ensemble
    # export PLOTTING="${DAS}"/plotting

    if [[ -n ${DAS_INITIALIZED:-} ]];then
        remove_from_PATH $DAS
        remove_from_PATH $SPEEDY
        remove_from_PATH $SPEEDY_BIN
        # remove_from_PATH $TOOLS
    fi
    export PATH=$DAS:$SPEEDY_BIN:$SPEEDY:$PATH
    # export PATH=$DAS:$SPEEDY_BIN:$SPEEDY:$TOOLS:$PATH

    # export PATH=${DAS}:${TOOLS}:${SPEEDY}:${LETKF}:${STATIONS}:${OBS}:${PARTICLE}:${ENSEMBLE}:${PLOTTING}:$PATH

    echo " - Configuring compilers"

    setup_modules_system
    export F90=${F90:-ifort} #  export   F90=${F90:-gfortran}
    echo " F90 = $F90"


    echo " - Setting archive folder"
    case $(hostname) in
        tux04|calc02|tux21)
            ARCH_DIR="/daten/model/acevedo/das_archive";;
            # ARCH_DIR="/home/acevedo/Desktop/speedyDA_arch";;
        # tux04|calc02) ARCH_DIR="/scratch/users/acevedo/archive/speedyDA_arch";;
        *) echo " unknown machine";  exit 1;;
    esac
    export ARCH_DIR
    echo " ARCH_DIR = $ARCH_DIR"
    STORE="$ARCH_DIR/test/$model"
    # case "$task" in
    #     check  )      STORE="$ARCH_DIR/test/$model";;
    #     launch|queue) STORE="$ARCH_DIR/batch2/$model";;
    # esac
    export STORE

    echo " - Setting up MPI " #(Machine dependent)
    case $(hostname) in
        tux04|calc02)
            case "$F90" in
                "gfortran") module load mpich2/gfortran-4.4.5/1.3.1;;
                "ifort"   ) module load mpich2/ifort-12.0.2/1.3.1;;
                *         ) error "Unknown compiler $F90";;
            esac ;;
        *) error " unknown machine";;
    esac
#    cp $DAS/tools/mpd.conf ~/.mpd.conf # Machine config file
#    chmod 600 ~/.mpd.conf
#    mpdboot -n 1
    export cpus=4
#    echo "   Running in $(mpdtrace)"

    export DAS_INITIALIZED=yes

    echo "--------------------------------------"
    echo " DAS enviroment properly set"
    echo "======================================"
    return 0
}

das_initialize #$@
#exit $?
