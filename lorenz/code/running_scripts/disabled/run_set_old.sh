#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create 1D- or 2D-set of free runs
#------------------------------------------------------
run_set(){
    funct_opening 3
    while getopts "m:r:" opt; do
        case $opt in
            m) local      run_mode="$OPTARG" ;;
            r) local reference_dir="$OPTARG"
                [[ -d "$reference_dir" ]] || error "Non existent reference_dir $reference_dir"
                ;;
        esac
    done
    shift $((OPTIND-1))

    local dataset_dir=$1; shift 1
    case $# in
        0)  ext_pars=0 ;;
        2)  ext_pars=1
            ext_par1_name=$1; ext_par1_span=$2
            ;;
        4)  ext_pars=2
            ext_par1_name=$1; ext_par1_span=$2;
            ext_par2_name=$3; ext_par2_span=$4;
            ;;
        *) error "Incorrect # of arguments $#";;
    esac

    # Create dataset dir
    create_archive_dir $dataset_dir
    cd $dataset_dir; mkdir runs

    dataset_kind="run_set"

    # report and save metainfo
    echo " Creating a set of runs"
    print_line 0
    set_par_file "dataset_kind"
    set_par_file "run_mode"
    set_par_file "dataset_dir"
    set_par_file "ext_pars"
    set_par_file "verity"
    print_line 0

    nprocess=0;
    case $run_mode in
        "free") run_free_set ;;
        "assi") run_assi_set ;;
        *     ) error "unsupported run_mode $run_mode";;
    esac

    # wait for a possible incomplete process batch
    for pid in ${pids[@]:-}; do
        wait $pid || error "Child process $pid crashed"
    done
    unset pids
#    wait

    # Tidy up dataset
    cat run_names.cfg | xargs -I% mv %.log $dataset_dir/runs/%
    mkdir config; mv *.cfg config
    funct_closing 3
}

run_free_set(){
    funct_opening 2

    # Configure run
    dataset_dim=$ext_pars
    set_par_file "dataset_dim"

    run_dir  (){ echo $dataset_dir/runs/$(run_name); }
    loop_core(){ run_free.sh $(run_dir); }

    # Send free runs
    case $dataset_dim in
        1)  par1_name=$ext_par1_name; par1_span=$ext_par1_span;
            run_set_1D
            ;;
        2)  par1_name=$ext_par1_name; par1_span=$ext_par1_span;
            par2_name=$ext_par2_name; par2_span=$ext_par2_span;
            run_set_2D
            ;;
    esac
    funct_closing 2
}

run_assi_set(){
    funct_opening 2

    # Configure run
    read run_free_set_dim < $reference_dir/config/dataset_dim.cfg
    dataset_dim=$(($run_free_set_dim + $ext_pars))
    if [[ $dataset_dim -gt 2 ]]; then
        error "Unsupported run set dimension $dataset_dim"
    fi
    set_par_file "run_free_set_dim"
    set_par_file "dataset_dim"

    run_assi_dir()  { echo $dataset_dir/runs/$(run_name); }
    loop_core(){ run_assi.sh $(run_assi_dir) $(run_free_dir); }

    # Send free runs
    case $run_free_set_dim in
        2)  read par1_name < $reference_dir/config/par1_name.cfg
            read par1_span < $reference_dir/config/par1_span.cfg
            read par2_name < $reference_dir/config/par2_name.cfg
            read par2_span < $reference_dir/config/par2_span.cfg
            run_free_dir (){ echo $reference_dir/runs/$(run_name); }
            run_set_2D
            ;;
        1)
            case $ext_pars in
                1)  par1_name=$ext_par1_name; par1_span=$ext_par1_span
                    read par2_name < $reference_dir/config/par1_name.cfg
                    read par2_span < $reference_dir/config/par1_span.cfg
                    run_free_dir (){
                        echo $reference_dir/runs/${par2_name}${par2_value}; }
                    run_set_2D
                    ;;
                *) error "run_free_set_dim-ext_pars combination $run_free_set_dim - $ext_pars Not ready yet";;
            esac
            ;;
        0)
            case $ext_pars in
                1)  par1_name=$ext_par1_name; par1_span=$ext_par1_span
                    run_free_dir(){ echo $reference_dir; }
                    run_set_1D
                    ;;
                *) error "run_free_set_dim-ext_pars combination $run_free_set_dim - $ext_pars Not ready yet";;
            esac
            ;;
        *) error "Unsupported run_free_set_dim $run_free_set_dim";;
    esac

    funct_closing 2
}

run_set_1D(){
    if [[ $dataset_dim -eq 1 ]]; then
        set_par_file par1_name; set_par_file par1_span
        create_par_values_file $par1_name $par1_span
        readarray -t par1_values < ${par1_name}_values.cfg
        par1_span_length=${#par1_values[@]}
        run_name(){ echo ${par1_name}${par1_value}; }
        print_line 0
    fi

    for ((i1 = 0 ; i1 < par1_span_length; i1++)); do
        par1_value=${par1_values[$i1]}
        eval "export ${par1_name}=\"$par1_value\""

        echo $(run_name) >> run_names.cfg

        echo $i1 >> ${par1_name}_value_pos.cfg
        if [[ $dataset_dim -eq 2 ]]; then
            echo $i2 >> ${par2_name}_value_pos.cfg
        fi

        { time -p loop_core; } &> $(run_name).log &
        pids[$nprocess]=$!
        echo " run $(run_name) [PID ${pids[$nprocess]}]"

        (( nprocess+=1 ))
        if [ $(( nprocess%cpus )) == 0 ]; then
            for pid in ${pids[@]};do
                wait $pid || error "Child process $pid crashed"
            done
            unset pids
        fi
#        (( nprocess+=1 )); [ $(( nprocess%cpus )) == 0 ] && wait
    done
#    return 0
}

run_set_2D(){
    set_par_file par1_name; set_par_file par1_span
    set_par_file par2_name; set_par_file par2_span
    create_par_values_file $par1_name $par1_span
    create_par_values_file $par2_name $par2_span
    readarray -t par1_values < ${par1_name}_values.cfg
    readarray -t par2_values < ${par2_name}_values.cfg
    par1_span_length=${#par1_values[@]}
    par2_span_length=${#par2_values[@]}
    print_line 0

    run_name(){
        echo ${par1_name}${par1_value}_${par2_name}${par2_value}; }

    for ((i2 = 0 ; i2 < par2_span_length; i2++)); do
        par2_value=${par2_values[$i2]}
        eval "export ${par2_name}=\"$par2_value\""

        run_set_1D $par1_name

    done
}

create_par_values_file(){
    par_name=$1
    par_span=$2

    case $par_name in
        # - Real valued parameters (must use numpy syntax)
        cycle_length|F_ampl|SNR|xlocal|link)
echo_real_par_values $par_span > ${par_name}_values.cfg
;;
        # - Integer valued parameters
cycles)
par_span_expanded=$(eval echo $par_span)
printf '%s\n' ${par_span_expanded[@]} > ${par_name}_values.cfg
;;
*) error "Unsupported parameter $par_name";;
        esac
        }

        set -o errexit # non-zero return values make the script halt
        set -o nounset # Unset variable expansion gives error
        source ${COM_DAS_DIR}/common_das_tools.sh
        run_set $@
        exit $?
