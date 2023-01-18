#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
#===================================================================
# L63_2s model simulation list
#===================================================================
model=L63_2s
task=${task:-launch}
sim_id=$1

source ./initialize_das.sh
source $DAS_DIR/common_tools.sh
source $DAS_DIR/das_tools.sh
check_dependencies
source $MODEL_DIR/default_config.sh
source $DAS_DIR/experiments.sh

L63_2s_par_set="uncoupled"
#L63_2s_par_set="enso"
#L63_2s_par_set="extratropical_Ocen_atm_strong_coupling"
#L63_2s_par_set="extratropical_Ocen_atm_weak_coupling"
#L63_2s_par_set="extratropical_Ocen_atm_weak_coupling"
set_pars $L63_2s_par_set

# exp_dir(){ echo "$STORE/$L63_2s_par_set/$exp_name"; }
# inflation_span="fspan(1.0,1.3,10)"

# experimental infl setting
exp_dir(){ echo "$STORE/link-${link}/${exp_name}__sqrt-scaled-infl"; }
infl_cycle_length_scaling="sqrt"
inflation_span="fspan(0.005,0.04,10)"

run_experiment $sim_id
