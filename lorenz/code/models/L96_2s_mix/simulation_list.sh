#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#===================================================================
# L96_2s_mix model simulation list (batch 2)
#===================================================================
model=L96_2s_mix
task=${task:-launch}
sim_id=$1

source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
check_dependencies

source $MODEL_DIR/default_config.sh
link="0.2d0" # [0-0.4)
# obs_operator="vsl0"
#export obs_operator="plus"
#export obs_operator="vsl0"
#export obs_operator="vsl1"


# exp_dir(){ echo "$STORE/link-${link}__obs_operator-$obs_operator/$exp_name"; }
# inflation_span="fspan(1.0,1.3,10)"


# experimental infl setting
exp_dir(){ echo "$STORE/link-${link}/${exp_name}__sqrt-scaled-infl"; }
infl_cycle_length_scaling="sqrt"
inflation_span="fspan(0.005,0.04,10)"

source $DAS_DIR/experiments.sh

run_experiment $sim_id
