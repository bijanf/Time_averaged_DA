#!/usr/bin/env bash
#set -x
#set -v
tada_exp_vs_cycle_length(){
    funct_opening 3
    dataset_dir=$1
    sampling_dir=$2
    cycle_length_span=$3
    update_mode=$4

    cycle_lengths=$(eval echo $cycle_length_span)

    nature_dir()  { echo $ARCH_DIR/$model/nature_cl${cycle_length}; }
    ens_free_dir(){ echo $ARCH_DIR/$model/ens_free_cl${cycle_length}; }
    ens_assi_dir(){ echo $ARCH_DIR/$model/ens_assi_cl${cycle_length}_$update_mode; }

    local par_index=0
    for cycle_length in ${cycle_lengths[@]}; do
        (( par_index += 1 ))
        par_ind=$(printf '%04d' ${par_index})

        echo "cycle_length -> $cycle_length"
        export cycle_length

#        echo "$par_value">${par_ind}_${par_name}.dat

        echo "- nature_run"
        { time -p yes | nature_run.sh $(nature_dir) $sampling_dir;}&> report.txt
        mv report.txt $(nature_dir)

        echo "- ens_free_run"
        { time -p yes | ens_free_run.sh $(ens_free_dir) $(nature_dir);}&> report.txt
        mv report.txt $(ens_free_dir)

        echo "- ens_assi_run"
        # export detail=1
        # export   plot="off"
        { time -p yes | ens_assi_run.sh $(ens_assi_dir) $(ens_free_dir);}&> report.txt
        mv report.txt $(ens_assi_dir)
    done
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
tada_exp_vs_cycle_length $@
exit $?
