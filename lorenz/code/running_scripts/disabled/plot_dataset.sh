#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
cd "$(dirname "${BASH_SOURCE[0]}")" # Go to folder where this file lies
source running_scripts/common_tools.sh
source ./initialize_das.sh

perform_task_in_dataset "plot"

perform_task_in_dataset(){
    task=$1
    ${cfg[dataset_kind]}.sh --task=$task "$dataset_dir"
}