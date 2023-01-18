#!/usr/bin/env bash
set -o nounset # Unset variable expansion gives error
cd "$(dirname "${BASH_SOURCE[0]}")" # Go to folder where this file lies
source running_scripts/common_tools.sh

echo "==============================================================="
echo " Initializing DAS"
echo "==============================================================="

echo " - DAS running config"

export   DBG=${debug:-off}
#export   DBG=${debug:-on}
export build=${build:-yes}
export batch_name=batch7

echo " - Folder structure"

export  CODE_DIR=$(pwd)
export   BIN_DIR=$CODE_DIR/../bin

source ./das_model_config.sh

case "$(hostname)" in
    negrito)
        ARCH_DIR="/home/jowa/Desktop/das_archive";;
#    tux04|calc01|calc02|calc03|calc04|tux21)
    calc01|calc02|calc03|calc04|tux21)
        ARCH_DIR="/daten/model/acevedo/das_archive";;
        # ARCH_DIR="$HOME/das_tmp";;
    tux04)
        ARCH_DIR="/daten/model/acevedo/das_archive";;
#        ARCH_DIR="/scratch/users/acevedo/archive/das_archive";;
    soroban|node???)
        ARCH_DIR="/scratch/acevedo/das_archive";;
    *)
        error "Unknown machine $(hostname)";;
#           ARCH_DIR=$CODE_DIR/../data ;;
esac
export ARCH_DIR
mkdir -p "$ARCH_DIR/logs"

export NCARG_COLORMAPS="$CODE_DIR/plotting/ncl_colormaps"

echo " - Operative system settings"

if [[ ! -n ${cpus:-} ]];then
    case "$(hostname)" in
        negrito)         cpus=2 ;;
        tux04  )         cpus=4 ;;
        tux21  )         cpus=8 ;;
        calc01 )         cpus=2 ;;
        calc02 )         cpus=4 ;;
        calc03 )         cpus=12;;
        calc04 )         cpus=6 ;;
#        calc01|calc03 |calc04) cpus=6 ;;
        soroban|node???) cpus=12;;
        *)               cpus=1 ;;
    esac
fi
export cpus

# Process data needed by soroban (model and process dependent !!)
# process_time=${process_time:-"24:00:00"}
process_time=${process_time:-"48:00:00"}
# process_time=${process_time:-"120:00:00"}
# mem_per_cpu=${mem_per_cpu:-2000} # In megabytes
mem_per_node=17500 # Max RAM for small soroban nodes
partition=${partition:-main}

# case "$(hostname)" in
#     calc03|calc04)
#         source /net/opt/system/modules/default/init/bash
# #        module unload python/2.7.5
#         module load ncl/6.1.2 ;;
# esac

clone=${clone:-yes}

export LANG=C # no localization features

echo "   Running in $(hostname) ($cpus cpus)"
echo "   model       = $model"
echo "   debug       = $DBG"
echo "   ARCH_DIR    = $ARCH_DIR"
echo "---------------------------------------------------------------"
echo " DAS enviroment properly set"
echo "==============================================================="
export DAS_INITIALIZED=yes
