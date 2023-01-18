#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#===================================================================
# L63 model simulation list
#===================================================================
model=L63
task=${task:-launch}
sim_id=$1

source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
check_dependencies
source $MODEL_DIR/default_config.sh
source $DAS_DIR/experiments.sh

exp_dir(){ echo "$STORE/$exp_name"; }

run_experiment $sim_id
