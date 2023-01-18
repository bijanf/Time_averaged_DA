#!/usr/bin/env bash
echo "======================================"
echo " Initializing DAS enviroment"
echo "======================================"

echo " Identifying server"
export machine=$(hostname)      
echo " Running in $machine"

echo "--------------------------------------"
echo " Initializing paths"
echo " - Absolute paths (Machine dependent)"

case "$machine" in
"tux04")
#    SCRATCH_DIR="/scratch/users/acevedo/scratch"
    ARCHIVE_DIR="/scratch/users/acevedo/speedyDA_ARCHIVE"
#    setup_modules_system
#    case "$tux04_compiler" in
#    "gfortran")
        module load mpich2/gfortran-4.4.5/1.3.1
        ;;
#    "ifort")
#        module load mpich2/ifort-12.0.2/1.3.1
#        ;;
#    *)
#        echo "Unknown compiler $tux04_compiler"
#        exit 1;;
#    esac
#    ;;
#"a41")
#    SCRATCH_DIR="/scratch/acevedo"
#    ARCHIVE_DIR="/scratch/acevedo/speedyDA_arch"
#    ;;
*)
    echo " unknown machine"
    exit 1;;
esac
export ARCHIVE_DIR

echo " - Relative paths"
# Find this script's folder
export DAS_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export   SPEEDY="${DAS_PATH}/speedy/model"
export PATH="${DAS_PATH}":$PATH
export PATH="${DAS_PATH}"/plotting:${PATH}
export PATH="${DAS_PATH}"/system:${PATH}
export PATH="${DAS_PATH}"/da_experiment:${PATH}
export PATH="${DAS_PATH}"/speedy/model/run:${PATH}
export PATH="${DAS_PATH}"/speedy/obs:${PATH}

echo " DAS enviroment properly set"
echo "======================================"

