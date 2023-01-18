#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create 1D- or 2D-set of free runs
#------------------------------------------------------
run_free_set(){
    funct_opening 3
    dataset_dir=$1

    # Create dataset dir
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir; mkdir runs

#    run_free_set.py $@

    # Configure run
    dataset_kind="run_set"

    
    # par_name[0]=$2;     par_span[0]=$3
    # if   [[ $# -eq 3 ]]; then
    #     dataset_dim=1
    #     par_name[1]="cycles"; par_span[1]="{${cycles}..${cycles}}"
    # elif [[ $# -eq 5 ]]; then
    #     dataset_dim=2
    #     par_name[1]=$4      ; par_span[1]=$5
    # else
    #     error "Usage: Incorrect # of arguments"
    # fi
    # printf '%s\n' "${par_span[@]}" > par_span1.cfg

    # run_free_name(){ echo ${par_name[1]}${par1_value}_${par_name[0]}${par0_value}; }
    # run_free_dir() { echo $dataset_dir/runs/$(run_free_name); }

    # for ((i = 0 ; i < 2; i++)); do
    #     # create par_values files
    #     case "${par_name[$i]}" in
    #         "cycle_length"|"F_ampl") # - Real valued parameters (must use numpy syntax)
    #             echo_real_par_values ${par_span[$i]} > ${par_name[$i]}_values.cfg
    #             ;;
    #         "cycles"      ) # - Integer valued parameters
    #             par_span_expanded=$(eval echo ${par_span[$i]})
    #             printf '%s\n' ${par_span_expanded[@]} > ${par_name[$i]}_values.cfg
    #             ;;
    #         *) error "Unsupported parameter ${par_name[$i]}";;
    #     esac
    #     readarray -t par${i}_values < ${par_name[$i]}_values.cfg
    #     # eval "par_span_length[$i]=$( cat ${par_name[$i]}_values.cfg | wc -l )"
    #     # eval "set_par_file \"par_span_length[$i]\""
    #     # eval "set_par_file \"par_span[$i]\""
    #     par_span_length[$i]=$( cat ${par_name[$i]}_values.cfg | wc -l )
    #     set_par_file par_span_length[$i]
    #     set_par_file par_span[$i]
    # done
    # printf '%s\n' "${par_span[@]}" > par_span2.cfg

    # # report and save metainfo
    # echo " Creating a run_free set"
    # print_line 0
    # set_par_file     "dataset_kind"
    # set_par_file     "dataset_dim"
    # for ((i = 0 ; i < dataset_dim; i++)); do
    #     set_par_file par_name[$i]
    #     set_par_file par_span[$i]
    #     set_par_file par_span_length[$i]
    # done
    # set_par_file     "dataset_dir"
    # set_par_file     "verity"
    # printf '%s\n' "${par_name[@]}" > par_name.cfg
    # printf '%s\n' "${par_span[@]}" > par_span.cfg
    # print_line 0

    # # Send free runs
    # nprocess=0;

    # for ((i1 = 0 ; i1 < par_span_length[1]; i1++)); do
    #     par1_value=${par1_values[$i1]}
    #     eval "export ${par_name[1]}=\"$par1_value\""

    #     for ((i0 = 0 ; i0 < par_span_length[0]; i0++)); do
    #         par0_value=${par0_values[$i0]}
    #         eval "export ${par_name[0]}=\"$par0_value\""

    #         echo " free run $(run_free_name)"
    #         echo $(run_free_name) >> run_names.cfg

    #         echo $i0   >> ${par_name[0]}_value_pos.list
    #         echo $i1   >> ${par_name[1]}_value_pos.list

    #         { time -p run_free.sh $(run_free_dir); } &> $(run_free_name).log &

    #         (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    #     done
    # done
    # wait

    # Tidy up dataset
    cat run_names.cfg | xargs -I% mv %.log $dataset_dir/runs/%
    mkdir config; mv *.cfg config
    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
run_free_set $@
exit $?
