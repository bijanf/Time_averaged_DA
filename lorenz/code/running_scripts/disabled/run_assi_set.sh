#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create 1D- or 2D-set of assimilation runs
#------------------------------------------------------
run_assi_set(){
    funct_opening 3
    local  dataset_dir=$1

    # Create dataset dir
    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir

    # Configure run
    dataset_kind="run_set"
    run_free_set=$2 # free run set (0- or 1-Dimensional )

    read run_free_set_dim < $run_free_set/config/dataset_dim.cfg
    case $# in
        2) dataset_dim=$run_free_set_dim          ;;
        4) dataset_dim=$(($run_free_set_dim + 1)) ;;
        6) dataset_dim=$(($run_free_set_dim + 2)) ;;
        *) error "Usage: Incorrect # of arguments";;
    esac
    [[ $dataset_dim -le 2 ]] || error "Unsupported dataset dimension $dataset_dim"

    if [ $run_free_set_dim -gt 0 ]; then
        readarray -t par_name < $run_free_set/config/par_name.cfg
        readarray -t par_span < $run_free_set/config/par_span.cfg
    fi
    case $run_free_set_dim in
        2)  run_free_name(){
                echo ${par_name[1]}${par1_value}_${par_name[0]}${par0_value}; }
	    run_free_dir(){ echo $run_free_set/runs/$(run_free_name); }
            ;;
        1)  par_name_tmp=${par_name[1]}
            par_value_tmp=$(cat $run_free_set/config/${par_name_tmp}_values.cfg)
            if [ $# -eq 4 ];then
                par_name[1]=$3; par_span[1]=$4
            fi
            run_free_name(){
                echo ${par_name_tmp}${par_value_tmp}_${par_name[0]}${par0_value}; }
	    run_free_dir(){ echo $run_free_set/runs/$(run_free_name); }
            ;;
        0)  run_free_dir(){ echo $run_free_set; }
            case $# in
                2)  error "Use run_assi.sh directly"       ;;
                4)  par_name[0]=$3; par_span[0]=$4
                    par_name[1]="cycles"; par_span[1]="{${cycles}..${cycles}}"
                    ;;
                6)  par_name[0]=$3; par_span[0]=$4
                    par_name[1]=$5; par_span[1]=$6
                    ;;
            esac
            ;;
        *) error "Usage: Incorrect # of arguments";;
    esac
    [[ $dataset_dim -le 2 ]] || error "Unsupported dataset dimension $dataset_dim"

    run_assi_name(){ echo ${par_name[1]}${par1_value}_${par_name[0]}${par0_value}; }
    run_assi_dir (){ echo  $dataset_dir/runs/$(run_assi_name); }

    # create par_values files
    for ((i = 0 ; i < 2; i++)); do
        case "${par_name[$i]}" in
            "cycle_length"|"SNR"|F_ampl) # - Real valued parameters (must use numpy syntax)
                echo_real_par_values ${par_span[$i]} > ${par_name[$i]}_values.cfg
                ;;
            "cycles"      ) # - Integer valued parameters
                par_span_expended=$(eval echo ${par_span[$i]})
                printf '%s\n' ${par_span_expended[@]} > ${par_name[$i]}_values.cfg
                ;;
            *) error "Unsupported parameter ${par_name[$i]}";;
        esac

        readarray -t par${i}_values < ${par_name[$i]}_values.cfg
        eval "par_span_length[$i]=$( cat ${par_name[$i]}_values.cfg | wc -l )"
        eval "set_par_file \"par_span_length[$i]\""

    done

    # report and save metainfo
    echo " Creating run_assi set"
    print_line 0
    set_par_file     "dataset_kind"
    set_par_file     "run_free_set_dim"
    set_par_file     "dataset_dim"
    for ((i = 0 ; i < dataset_dim; i++)); do
        set_par_file "par_name[$i]"
        set_par_file "par_span[$i]"
       # set_par_file "par1_span_length[$i]"
    done
    set_par_file     "dataset_dir"
    set_par_file     "verity"
    printf '%s\n' "${par_name[@]}" > par_name.cfg
    printf '%s\n' "${par_span[@]}" > par_span.cfg
    print_line 0

    # Send assi runs
    nprocess=0;

    for ((i1 = 0 ; i1 < par_span_length[1]; i1++)); do
        par1_value=${par1_values[$i1]}
        eval "export ${par_name[1]}=\"$par1_value\""

        for ((i0 = 0 ; i0 < par_span_length[0]; i0++)); do
            par0_value=${par0_values[$i0]}
            eval "export ${par_name[0]}=\"$par0_value\""

            echo " assimilation run $(run_assi_name)"
            echo $(run_assi_name) >> run_names.cfg

            echo $i0   >> ${par_name[0]}_value_pos.list
            echo $i1   >> ${par_name[1]}_value_pos.list

            { time -p run_assi.sh $(run_assi_dir) $(run_free_dir); } \
                &> $(run_assi_name).log &

            (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
        done
    done
    wait

    # Tidy up dataset
    cat run_names.cfg | xargs -I% mv %.log $dataset_dir/runs/%
    mkdir config; mv *.cfg config
    funct_closing 3
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
run_assi_set $@
exit $?
