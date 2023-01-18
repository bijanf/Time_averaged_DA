#!/usr/bin/env bash
#=========================================================================
#> @brief Stats launcher
#=========================================================================
das_stats(){
    funct_opening 3
    dataset_dir=$1
    dataset_dir="$(absolute_path "$dataset_dir")"

    read dataset_kind < $dataset_dir/config/dataset_kind.cfg
    ${dataset_kind}_stats.sh $dataset_dir

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh

das_stats $@
exit $?


