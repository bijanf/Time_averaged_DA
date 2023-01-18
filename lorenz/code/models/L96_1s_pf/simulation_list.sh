#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#===================================================================
# L96 model simulation list
#===================================================================
model=L96_1s_pf
task=${task:-launch}
sim_id=$1

source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
check_dependencies
source $MODEL_DIR/default_config.sh
source $DAS_DIR/experiments.sh

F_ampl="0.0d0"
exp_dir(){ echo "$STORE/F_ampl-${F_ampl}/$exp_name"; }

run_experiment $sim_id