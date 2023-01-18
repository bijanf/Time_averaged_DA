#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
cd "$(dirname "${BASH_SOURCE[0]}")" # Go to folder where this file lies
source running_scripts/common_tools.sh
#source running_scripts/metainfo_tools.sh

declare -A cfg

usage(){ cat << EOF
Data assimilation system launching script.
Usage: $(basename $0) [--sim_id=*] [--model=*] [--verity=*] [--debug] [--no_copy] [-h|--help]
- model        : dynamical system name
- sim_id       : Identifier from ./\$model/model_script.sh
- verity_level : 0/1 (see run.sh)
*/^: Required/optional argument
EOF
exit 1
}
# # Estanis' automatic sage function for getopts (to be adpated to getopt)
# usage() {
#     echo "Usage: $(basename $0) [flags]"
#     echo "Flags is one of:"
#     sed -n '/^while getopts/,/^done/  s/^\([^)]*\)[^#]*#\(.*$\)/\1 - \2/p' $0
# }

#=============================
#> @brief das launcher
#=============================
das(){
    funct_opening 7

    # task=$1; shift 1;;

    long_opts="help,task:,sim_id:,sim_list:,model:,verity:,f90:,debug,clone:,build:,calc:,stat:,plot:,docu:,replot,cpus:,process_time:,mem_per_node:,partition:,verbose:"
    PARSED_ARGS=$(getopt -n "$0" -o h --long "$long_opts" -- "$@")
    eval set -- "$PARSED_ARGS"
    while true; do
        case "$1" in
            -h|--help     )            usage            ;;
            --task        )             task=$2; shift 2;;
            --model       )            model=$2; shift 2;;
            --sim_id      )           sim_id=$2; shift 2;;
            --sim_list    )         sim_list=$2; shift 2;;
            --verity      )   export  verity=$2; shift 2;;
            --f90         )              F90=$2; shift 2;;
            --debug       )            debug=on; shift 1;;
            --clone       )            clone=$2; shift 2;;
            --build       )            build=$2; shift 2;;
            --calc        ) global_calc_flag=$2; shift 2;;
            --stat        ) global_stat_flag=$2; shift 2;;
            --plot        ) global_plot_flag=$2; shift 2;;
            --docu        ) global_docu_flag=$2; shift 2;;
            --replot      )          replot=yes; shift 1;;
            --cpus        )             cpus=$2; shift 2;;
            --process_time)     process_time=$2; shift 2;;
            --mem_per_node)      mem_per_cpu=$2; shift 2;;
            --partition   )        partition=$2; shift 2;;
            --verbose     )   export verbose=$2; shift 2;;
            --            )             shift 1; break  ;;
        esac
    done

    dataset_dir="${1:-}"
    dataset_dir="${dataset_dir%/}" # strip possible leading /

    source ./initialize_das.sh
    check_dependencies

    case "$task" in
        "test"            ) das_test ;;
        "launch"          ) das_launch ;;
        "queue_soroban"   ) queue_soroban ;;
        "queue_met_fub"   ) queue_met_fub ;;
        "queue_in_calc03" ) queue_in_calc03  ;;
        "queue_in_calc04" ) queue_in_calc04  ;;
        "queue_in_soroban") queue_in_soroban ;;
        "sel_plots"       ) das_sel_plots ;;
        "doxydoc"         ) create_doxygen_document ;;
        #"clean"           ) das_clean  ;;
        "clean"           ) perform_task_in_dataset "clean"  ;;
        "stat"            ) perform_task_in_dataset "stat"  ;;
        # "postStat"        ) perform_task_in_dataset "postStat" ;;
        "plot"            ) perform_task_in_dataset "plot"  ;;
        "docu"            ) perform_task_in_dataset "docu"  ;;
        "view"            ) perform_task_in_dataset "view"  ;;
        *                 ) error "Unknown task $task"  ;;
    esac

    funct_closing 7
}

perform_task_in_dataset(){
    task=$1
    get_config_file "$dataset_dir/config/dataset_kind.cfg"
    ${cfg[dataset_kind]}.sh --task=$task --replot=${replot:-} "$dataset_dir"
}

das_test(){
    funct_opening 6

    export global_plot_flag=yes
    export global_docu_flag=yes
    
    export clone="no"
    export verity=0
    export verbose=${verbose:-1}
    export STORE="$ARCH_DIR/test"
    sim_id=${sim_id:-"test"}

    ( my_time yes | "$MODEL_DIR/model_script.sh" "$sim_id"
        echo $SECONDS > fast_run_length.dat )

    read fast_run_length < fast_run_length.dat
    real_run_length=$(echo ${fast_run_length}*100/3600|bc -l)
    fast_run_length=$(printf '%f3' $fast_run_length)
    real_run_length=$(printf '%f3' $real_run_length)
    rm fast_run_length.dat
    echo " Fast run duration: $fast_run_length seconds"
    echo " Estimated real run duration: $real_run_length hours"

    funct_closing 6
}

job_name(){ echo "${model}__${sim_id}__$(date +%y_%m_%d)__$(hostname)";}

das_launch(){
    funct_opening 6

    clone_code

    export clone="yes"
    export STORE="$ARCH_DIR/$batch_name"

    log_file="$ARCH_DIR/logs/$(job_name)_launch.log"
    nohup "$MODEL_DIR/model_script.sh" "$sim_id" > "$log_file" &
    sim_pid=$!
    echo " Model $model"
    echo " Simulation ID $sim_id (PID $sim_pid)"
    echo " To check the progress execute"
    echo " tail -f $log_file"

    funct_closing 6
}

#-----------------------------------------------------
# Put a process into met-fub slurm system queue
# memory values are given im MBytes
#------------------------------------------------------
queue_met_fub(){
    funct_opening 6

    clone_code

    export clone="yes"
    export STORE="$ARCH_DIR/$batch_name"

    if [[ -n ${sim_list:-} ]];then
        sim_list_exp=($(eval "echo ${sim_list}"))
    else
        sim_list_exp=$sim_id
    fi

    for sim_id in ${sim_list_exp[@]}; do

        log_file="$ARCH_DIR/logs/$(job_name)_slurm-%j.log"
        slurm_script="sbatch_script.sh"

        cat > $slurm_script <<EOF
#!/usr/bin/env bash
#SBATCH --job-name="$(job_name)"
#SBATCH --output="${log_file}"
#SBATCH --error="${log_file}"
#SBATCH --mail-user=walter.acevedo@met.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1-1
#SBATCH --ntasks-per-node=$cpus
#SBATCH --partition=$partition

echo " Simulation ID $sim_id"
bash "$MODEL_DIR/model_script.sh" $sim_id
EOF

sbatch $slurm_script #| tee "$ARCH_DIR/logs/$(job_name)-slurm.jobid"

echo " Model $model"
echo " Simulation ID $sim_id"
echo " To check the progress execute"
log_file="$ARCH_DIR/logs/$(job_name)_slurm-job_number.log"
echo " tail -f $log_file"

    done

    funct_closing 6
}

#-----------------------------------------------------
# Put a process into soroban slurm system queue
# memory values are given im MBytes
#------------------------------------------------------
queue_soroban(){
    funct_opening 6

    export STORE="$ARCH_DIR/$batch_name"
    mkdir -p $STORE

    if [[ -n ${sim_list:-} ]];then
        sim_list_exp=($(eval "echo ${sim_list}"))
    else
        sim_list_exp=$sim_id
    fi

    for sim_id in ${sim_list_exp[@]}; do

        log_file="$ARCH_DIR/logs/$(job_name)_slurm-%j.log"
        err_file="$ARCH_DIR/logs/$(job_name)_slurm-%j.err"
        slurm_script="sbatch_script.sh"
        cat > $slurm_script <<EOF
#!/usr/bin/env bash
#SBATCH --job-name="$(job_name)"
#SBATCH --output="${log_file}"
#SBATCH --error="${err_file}"
#SBATCH --mail-user=walter.acevedo@met.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1-1
#SBATCH --ntasks-per-node=$cpus
#SBATCH --mem=$mem_per_node
#SBATCH --time=$process_time
#SBATCH --partition=$partition

echo " Simulation ID $sim_id"
echo " STORE dir: $STORE"

bash "$MODEL_DIR/model_script.sh" $sim_id
EOF

sbatch $slurm_script #| tee "$ARCH_DIR/logs/$(job_name)-slurm.jobid"

# other previously used options
#SBATCH --mem-per-cpu=$mem_per_cpu

# sbatch \
#     --job-name="$(job_name)" \
#     --output="$log_file" \
#     --error="${log_file}.out" \
#     --mail-user=walter.acevedo@met.fu-berlin.de \
#     --mail-type=end \
#     --ntasks=$cpus \
#     --mem-per-cpu=$mem_per_cpu \
#     --time=$process_time \
#     --partition=$partition \
#     "$MODEL_DIR/model_script.sh" $sim_id

echo " Model $model"
echo " Simulation ID $sim_id"
echo " To check the progress execute"
echo " tail -f $log_file"

    done

    funct_closing 6
}

#----------------------------------------------------------------------
# Intermediate Functions that allow queuing in calc03/04 and soroban
# without interactively logging in those machines
#----------------------------------------------------------------------

queue_in_met_fub(){
    ssh calc01 ". /etc/profile; . ~/.bashrc; $CODE_DIR/das.sh --task=queue_met_fub --model=$model --sim_id=$sim_id --cpus=$cpus"; }

queue_in_calc03(){ export cpus=12; queue_in_met_fub; }
queue_in_calc04(){ export cpus=6 ; queue_in_met_fub; }

queue_in_soroban(){
    funct_opening 6

    # ARCH_THERE=/scratch/acevedo/das_copies
    # Finding free temporary folder
    TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das_copy${count}"; }
    count=1 # Code copy number
    while [[ -d $(TMP_CODE_DIR) ]]; do (( count++ )); done
    create_code_copy "$(TMP_CODE_DIR)"
    ( cd "$ARCH_DIR/tmp"; tar -cvf tmp_code.tar.gz das_copy${count}>/dev/null )

    echo " - Moving code copy to soroban"
    ssh soroban "mkdir -p $ARCH_THERE"
    scp "$ARCH_DIR/tmp/tmp_code.tar.gz" "soroban:$ARCH_THERE" > /dev/null
    ssh soroban "cd $ARCH_THERE; tar -xvf tmp_code.tar.gz > /dev/null"

    echo " - Submiting process to queue"
    ssh soroban "export PATH=/home/acevedo/bin:\$PATH; $ARCH_THERE/das_copy${count}/code/das.sh --task=queue_soroban --clone=no --model=$model --sim_id=$sim_id"

    funct_closing 6
}

das_clean(){
    funct_opening 6

    echo " Cleaning the system makes queued jobs crash !!!"
    read -p " Do you want to continue cleaning? y/* " yn
    case $yn in
        [Yy]*)
            echo " Removing temporary files"
            rm -fr "$ARCH_DIR/tmp"
            rm -fr "$ARCH_DIR/test"
            # find "$ARCH_DIR/tmp"  -delete &
            # find "$ARCH_DIR/test" -delete &
            # wait
            ;;
        *    ) echo " Cleaning aborted" ;;
    esac

    funct_closing 6
}

das $@
exit $?
