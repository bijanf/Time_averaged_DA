#!/usr/bin/env bash

config_run_set_parameters(){

    if [[ -n ${ext_par1_name:-} ]] && [[ -n ${ext_par2_name:-} ]]; then
        ext_pars=2
    elif [[ -n ${ext_par1_name:-} ]]; then
        ext_pars=1
    else
        ext_pars=0
    fi

    dataset_dim=$(($ref_set_dim + $ext_pars))
    [[ $dataset_dim -gt 2 ]] &&  error "Unsupported run set dimension $dataset_dim"

    case $ref_set_dim in
        0)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    ref_run_dir(){ echo "$ref_set_dir"; }
                    ;;
                2)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    par2_name=$ext_par2_name
                    par2_span=$ext_par2_span
                    par2_unit=$ext_par2_unit
                    ref_run_dir(){ echo "$ref_set_dir"; }
                    ;;
                *)  error "Unsupported ref_set_dim-ext_pars combination ${ref_set_dim}-${ext_pars}";;
            esac
            ;;
        1)
            case $ext_pars in
                1)
                    par1_name=$ext_par1_name
                    par1_span=$ext_par1_span
                    par1_unit=$ext_par1_unit
                    read par2_name < $ref_set_dir/config/par1_name.cfg
                    read par2_span < $ref_set_dir/config/par1_span.cfg
                    read par2_unit < $ref_set_dir/config/par1_unit.cfg
                    ref_run_dir (){ echo "$ref_set_dir/runs/${par2_name}-${par2_value}"; }
                    ;;
                0)
                    read par1_name < $ref_set_dir/config/par1_name.cfg
                    read par1_span < $ref_set_dir/config/par1_span.cfg
                    read par1_unit < $ref_set_dir/config/par1_unit.cfg
                    ref_run_dir (){ echo "$ref_set_dir/runs/${par1_name}-${par1_value}"; }
                    ;;
                *) error "Unsupported ref_set_dim-ext_pars combination ${ref_set_dim}-${ext_pars}";;
            esac
            ;;
        2)
            read par1_name < $ref_set_dir/config/par1_name.cfg
            read par1_span < $ref_set_dir/config/par1_span.cfg
            read par1_unit < $ref_set_dir/config/par1_unit.cfg
            read par2_name < $ref_set_dir/config/par2_name.cfg
            read par2_span < $ref_set_dir/config/par2_span.cfg
            read par2_unit < $ref_set_dir/config/par2_unit.cfg
            ref_run_dir (){ echo "$ref_set_dir/runs/$(run_name)"; }
            ;;
        *)
            error "Unsupported run_free_set_dim $run_free_set_dim";;
    esac
}

calc_nature_run_set(){
    funct_opening 2

    echo " Creating a set of nature runs"
    cd "$dataset_dir"

    loop_core(){
        echo $(run_name) >> config/run_names.cfg
        # echo $(run_name) >> ../logs/calc_consumption.log

        echo $i1 >> config/par1_value_pos.cfg
        [[ $dataset_dim -eq 2 ]] && echo $i2 >> config/par2_value_pos.cfg

        single_calc_run_command >/dev/null &
        run_pid=$!

        pids[$i_run]=$run_pid
        echo " run $(run_name) [PID ${pids[$i_run]}]"

        if (( i_run%cpus == 0 )); then
            wait_for_child_processes #"strict"
        fi
    }
    loop_over_set
    wait_for_child_processes #"strict" # of an incomplete batch

    # Distrubute run calc_consumption.logs
    echo "Run ${task} consumption" > logs/${task}_consumption.log
    loop_core(){
        cat "$(run_name).log" >> logs/${task}_consumption.log
        cat "$(run_name).log" > \
            "$(run_dir)/logs/${task}_consumption.log"
        rm  "$(run_name).log"
  #------------------------------------------------------------------
  # next line is simpler, but in some systems mv shows permission problems
  #  mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log";
  #------------------------------------------------------------------
    }
    loop_over_set

    funct_closing 2
}

calc_free_run_set(){
    funct_opening 2

    echo " Creating a set of free ensemble runs"
    calc_ens_run_set

    funct_closing 2
}

calc_assi_run_set(){
    funct_opening 2

    echo " Creating a set of ensemble runs with data assimilation"
    calc_ens_run_set

    funct_closing 2
}

calc_ens_run_set(){
    #funct_opening 2

    loop_core(){
        echo " - $(run_name)"
        echo $(run_name) >> config/run_names.cfg
        # echo $(run_name) >> ../logs/calc_consumption.log

        echo $i1 >> config/par1_value_pos.cfg
        [[ $dataset_dim -eq 2 ]] && echo $i2 >> config/par2_value_pos.cfg

        single_calc_run_command > /dev/null

    }
    loop_over_set

    # Distrubute run calc_consumption.logs
    loop_core(){
        mv "$(run_name).log" "$(run_dir)/logs/calc_consumption.log"; }
    loop_over_set

    #funct_closing 2
}
