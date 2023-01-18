#!/usr/bin/env bash
HERE="$(dirname "${BASH_SOURCE[0]}")"
source "$HERE/metainfo_tools.sh"
source "$HERE/report_tools.sh"

# next line excludes this file from doxydoc to make calling graphs cleaner
#> @cond

funct_opening(){
    local importance=${1:-0}
    local funct_name=${2:-"$(calling_function)"}

    print_line $importance
    echo " STARTING   ${funct_name}"
    print_line $importance
    (( $importance > 4 )) && echo ' '

    return 0
}

funct_closing(){
    local importance=${1:-0}
    local funct_name=${2:-"$(calling_function)"}

    (( $importance > 4 )) && echo ' '
    print_line $importance
    echo " NORMAL END ${funct_name}"
    print_line $importance

    return 0
}

print_line(){
    local importance=${1:-0}
    case "$importance" in
        0)  echo ":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::" ;;
        1)  echo "---------------------------------------------------------------" ;;
        2)  echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" ;;
        3)  echo "===============================================================" ;;
        4)  echo "---------------------------------------------------------------"
            echo "---------------------------------------------------------------" ;;
        5)  echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" ;;
        6)  echo "==============================================================="
            echo "===============================================================" ;;
        7)  echo "###############################################################"
            echo "###############################################################" ;;
    esac
}

#=======================================================================
#> @brief Error exit function
#> @param $1: string containing descriptive error message
#=======================================================================
error() {
    error_message="${1:-}"
    info=$(caller 0)        # Calling function info
    OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
    info_array=( $info )
    IFS="$OLD_IFS"          # restoring original file separator

    funct_line=${info_array[0]}
    funct_name=${info_array[1]}
    funct_file=${info_array[2]}

    echo_message(){
        print_line 1
        echo " Error in function '${funct_name}'"
        [ -z "$error_message" ] || echo " $error_message"
        echo " (file $(trim $(basename $funct_file)), line ${funct_line})"
        print_line 1
    }

    echo_message 1>&2; exit 1
}

#=======================================================================
#> @brief Creates archive_dir or asks if it must be replaced
#=======================================================================
create_archive_dir(){
    [ $# -eq 1 ] || error "Usage: create_archive_dir archive_dir"
    archive_dir=$1

    if [ -d "$archive_dir" ]; then
        echo " There is already a dataset called $( basename $archive_dir )"
        echo " ($archive_dir)"
        read -p " Do you want to replace it? y/* " yn
        case $yn in
            [Yy]*) rm -fr "$archive_dir"; mkdir -p "$archive_dir";;
            *    ) echo " Program was not allowed to overwrite dataset"
                echo    " Stopping program"; return 1
                ;;
        esac
    else
        mkdir -p "$archive_dir"
    fi
}

perform_simulation_tasks(){

    [[ ${global_plot_flag:-} == yes ]] && flag[plot]=yes
    [[ ${global_docu_flag:-} == yes ]] && flag[docu]=yes
	    
    flag[calc]=${flag[calc]:-yes}
    flag[stat]=${flag[stat]:-yes}
    flag[plot]=${flag[plot]:-no}
    flag[docu]=${flag[docu]:-no}
    flag[view]=${flag[view]:-no}
    flag[clean]=${flag[clean]:-no}

    if [[ -n ${only_task:-} ]]; then
        flag=(); flag[$only_task]=yes
    fi

    (( $verbose > 0 )) && funct_opening 3 $(calling_function)

    if [[ ${flag[calc]:-} == "yes" ]];then

        echo " dataset_dir = "
        echo " $dataset_dir"
        create_archive_dir "$dataset_dir"
        cd "$dataset_dir"; mkdir logs config;
        config_function > logs/config.log
        touch logs/config.success
        #[[ ${cfg[dataset_kind]} != run ]] && env > logs/env.log
        #cp $CODE_DIR/models/$model/default_config.sh ./logs
        #cp $CODE_DIR/models/$model/model_core.f90    ./logs
    fi

    for task in calc stat plot docu view clean; do
        if [[ ${flag[$task]:-} == "yes" ]]; then
            cd "$dataset_dir"
            rm -f "${dataset_dir}/logs/${task}.success"
            get_config; parse_dataset_kind

            if (( $verbose > 0 ));then
                ${task}_function \
                    1> >(tee logs/${task}.log) \
                    2> >(tee logs/${task}.error 1>&2)
            else
                ${task}_function \
                    1>       logs/${task}.log \
                    2> >(tee logs/${task}.error 1>&2)
            fi
            touch "${dataset_dir}/logs/${task}.success"
        fi
    done

    (( $verbose > 0 )) && funct_closing 3 $(calling_function)
    return 0
}

model_make(){
    if [[ $clone == yes ]];then
        create_code_copy "$(exp_dir)"
        cd "$(exp_dir)/code"
        source ./initialize_das.sh
    fi

    [[ $build == yes ]] && ( das_make_all $model )
}


check_dependencies(){
    funct_opening 3

    printf " - bash "
    (( ${BASH_VERSINFO[0]} >= 4 )) && echo "OK" || \
        error "bash major version should be at least no. 4"

    printf " - getopt "
    getopt_test=$( getopt --test > /dev/null; echo $? )
    (( $getopt_test == 4 )) && echo "OK" || error "GNU enhanced getopt not available"

    printf " - ncl "
    command -v ncl >/dev/null && echo "OK" || error "ncl not available"

    # printf " - ncra "
    # command -v ncra >/dev/null && echo "OK" || error "nco not available"

    printf " - cdo "
    command -v cdo >/dev/null && echo "OK" || error "cdo not available"

    printf " - python "
    command -v python >/dev/null && echo "OK" || error "python not available"

    # printf " - pdflatex "
    # command -v pdflatex >/dev/null && echo "OK" || error "pdflatex not available"

    funct_closing 3
}

strip_leading_zeros () # Strip possible leading zero(s), since otherwise
{                      # Bash will interpret such numbers as octal values.
  shopt -s extglob     # Turn on extended globbing.
  local val=${1##+(0)} # Use local variable, longest matching series of 0's.
  shopt -u extglob     # Turn off extended globbing.
  echo "$val"
#  _strip_leading_zero2=${val:-0}
#                       # If input was 0, return 0 instead of "".
}    

#view_function(){
    #funct_opening 2

    #[[ -f logs/docu.success ]] || \
        #$(basename $0) --task=docu "$dataset_dir"
    ## [[ -f logs/docu.success ]] || docu_function

    #report_file=$(find . -maxdepth 1 -name "*.pdf")
    #[[ -n ${report_file:-} ]] && okular $report_file
    #funct_closing 2
#}


wait_for_child_processes(){
    policy=${1:-relaxed}
    if [[ $policy == strict ]]; then
        for pid in ${pids[@]:-}; do
            wait $pid || ret_code=$?
            if [[ ${ret_code:-0} != 0 ]]; then
                info=$(caller 0)        # Calling function info
                OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
                info_array=($info)
                IFS="$OLD_IFS"          # restoring original file separator

                funct_line=${info_array[0]:-}
                funct_name=${info_array[1]:-}
                funct_file=${info_array[2]:-}

                echo_message(){
                    print_line 1
                    echo " Wait Error: Child process $pid crashed"
                    echo " (file $(trim $(basename $funct_file)), line ${funct_line})"
                    print_line 1
                }

                echo_message 1>&2; exit 1
            fi
        # error "Child process $pid crashed"
        done
    else
        for pid in ${pids[@]:-}; do
            wait $pid || echo "Child process $pid crashed"
        done
    fi
    unset pids
}


absolute_path(){
    local path=$1
    # Make dataset_dir absolute if it is relative

    if [[ "$path" = */ ]]; then
        path="${path%?}" # remove leading / if present
    fi
    if ! [[ "$path" = /* ]]; then
        path="$(pwd)/$path"
    fi
    echo "$path"
}

relpath(){
    python -c "import os.path; print os.path.relpath('$1','${2:-$PWD}')"
}

calling_function(){
    info=$(caller 1)        # Calling function info
    OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
    info_array=( $info )
    IFS="$OLD_IFS"          # restoring original file separator

#    funct_line=${info_array[0]}
    funct_name=${info_array[1]}
#    funct_file=${info_array[2]}
    echo $funct_name
}

#------------------------------------------------------
#> @brief Find the line starting with string1 in file
#>       and replace it (the whole line) with string2
#------------------------------------------------------
replace(){
    [[ $# -eq 3 ]] || error "Usage: replace string1 string2 file"
    sed -i "s/$(echo "$1" | sed -e 's/\([[\/.*]\|\]\)/\\&/g').*/$(echo "$2" | sed -e 's/[\/&]/\\&/g')/g" "$3"
}


trim() {
    local var=$@
    var="${var#"${var%%[![:space:]]*}"}"   # remove leading whitespace characters
    var="${var%"${var##*[![:space:]]}"}"   # remove trailing whitespace characters
    echo -n "$var"
}


# Cleaner way to do the following
# scalars=(${scalars[@]##*/}); scalars=(${scalars[@]%.*})
barename(){
    local paths=($@)
    local names=(${paths[@]##*/}); names=(${names[@]%.*})
    echo ${names[@]}
}

# barename(){
#     array_input_name=$1
#     code_line1="array_output=(\${${array_input_name}[@]##*/})"
#     code_line2="array_output=(\${array_output[@]%.*})"
#     code_line3="echo \${array_output[@]}"
#     # echo $code_line1; echo $code_line2; echo $code_line3
#     eval $code_line1; eval $code_line2; eval $code_line3
# }

order_array(){
    array_input_name=$1
    code_line="printf \"%s \n\" \${$array_input_name[@]} | sort -f"
    # echo $code_line
    eval $code_line
}

## mkdir and cd together ##
mcd(){ test -e $1 || mkdir $1; cd $1; }

process_age(){
    process_pid=$1
    echo `date +%s` - `stat -t /proc/$process_pid | awk '{print $14}'` | bc
}


#----------------------------------------------------------
# this function was defined so that unset variables lead to
# an error exit under the bash configuration "set -ue"
#
# notice that the following simpler way to multiply
# "cycle_length=$(echo "print ${cycle_steps}*${dt}"| python)"
# does not make the main shell exit if a variable is unset
#------------------------------------------------------------
product(){
#    echo "print $1 * $2" | python > product.tmp
    echo "asciiwrite(\"product.tmp\",$1 * $2)" | ncl > /dev/null
    read product < product.tmp
    rm product.tmp
    eval "$3=$product"
}


my_time(){
    /usr/bin/time -f " User:%U  System:%S  Wallclock:%E  Data:%Dkb \n MaxRAM:(%M/4)kb  Inputs:%I  Outputs:%O" \
        $@
}
# predecesors
#export timer="time -p"        # just time consumption
# export timer="/usr/bin/time" # time and Max Resident set size (RAM)
#                                  Note that RAM value is 4 times bigger
#export timer="/usr/bin/time -v" # many more data
# export time_format="user:%U system:%S wallclock:%E data:%Dkb maxRAM:(%M/4)kb inputs:%I outputs:%O"


# silencer wrapper for cdo
my_cdo(){
    cdo -L $@ 2> cdo.log  || ret_code=$?
    if [[ ${ret_code:-0} != 0 ]]; then
        info=$(caller 0)        # Calling function info
        OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
        info_array=($info)
        IFS="$OLD_IFS"          # restoring original file separator

        funct_line=${info_array[0]:-}
        funct_name=${info_array[1]:-}
        funct_file=${info_array[2]:-}

        echo_message(){
            print_line 1
            echo " cdo error" # in $function_name"
            echo " (file $(trim $(basename $funct_file)), line ${funct_line})"
            echo " command = \"cdo -L $@\""
            print_line 1
            cat cdo.log
            print_line 1
        }

        echo_message $@ 1>&2; exit 1
    fi
    rm cdo.log
}

CDO(){
    inDir="$1"; outDir="$2"; fullname=$3; oper1=$4; oper2=${5:-}
    filename=${fullname%.*}

    if (( $# >= 4 )); then
	if [[ $oper1 == "import_binary" ]];then
	    my_cdo -f nc import_binary \
		"${inDir}/${filename}.ctl" "$outDir/${filename}.nc"
	    (( $# == 5 )) && error "No second operator allowed after import_binary"
	else
	    my_cdo ${oper1} "${inDir}/${filename}.nc" "$outDir/${filename}_${oper1}.nc"
	fi
    fi

    if (( $# == 5 )); then
        my_cdo ${oper2} \
	    "$outDir/${filename}_${oper1}.nc" \
	    "$outDir/${filename}_${oper1}_${oper2}.nc"
        rm  "$outDir/${filename}_${oper1}.nc"
    fi
}

#===========================================================
#> @brief Look for Grads control files and try to write
#> the associated fortran binary files in netcdf using CDO
#===========================================================
export_fortran_binaries_as_netcdf(){
    #local dir_path=${1:-"./"}
    local   option=${1:-none}
    #cd $dir_path

    binary_files=($(find . -name "*.ctl"))
    if [[ -n ${binary_files:-} ]];then
        binary_names=($(barename ${binary_files[@]}))
        for binary_name in ${binary_names[@]}; do
            my_cdo -f nc import_binary \
                ${binary_name}.ctl ${binary_name}.nc

            if [[ $option == erase ]];then
                rm ${binary_name}.{grd,ctl}
            fi
        done
    else
        echo "No fortran binaries to export"
    fi
}

my_pdflatex(){
    local latexfile=$1
    echo " Compiling latex file $latexfile"
    pdflatex -halt-on-error "$latexfile" &> pdflatex.log  || ret_code=$?
    if [[ ${ret_code:-0} != 0 ]]; then
        info=$(caller 0)        # Calling function info
        OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
        info_array=( $info )
        IFS="$OLD_IFS"          # restoring original file separator

        funct_line=${info_array[0]}
        funct_name=${info_array[1]}
        funct_file=${info_array[2]}

        echo_message(){
            print_line 1
            echo " pdflatex error" # in $function_name"
            echo " (file $(trim $(basename $funct_file)), line ${funct_line})"
            print_line 1
            cat pdflatex.log
            print_line 1
        }

        echo_message 1>&2; exit 1
    fi
    rm pdflatex.log
    rm *.aux; rm *.log; #rm *.tex
    #latex ${doc_name}.tex; dvipdf ${doc_name}.dvi
    #dvips ${doc_name}.dvi; rm *.dvi; ps2pdf ${doc_name}.ps
}

#===================================================
#> @brief Run NCL silently and generate an error
#> exit if the word fatal is present in the log
#===================================================
my_ncl(){
    PARSED_ARGS=$(getopt -n "my_ncl" -o x --long "logfile:" -- "$@")
    eval set -- "$PARSED_ARGS"
    while true; do
        case "$1" in
            -x        ) ncl_verbose=yes; shift 1;;
            --logfile ) ncl_logfile=$2 ; shift 2;;
            --        )        shift 1 ; break  ;;
        esac
    done
    ncl_verbose=${ncl_verbose:-no}
    ncl_logfile=${ncl_logfile:-"ncl.log"}

    if [[ $ncl_verbose == yes ]]; then
        ncl -x $@
    else
        ncl $@ &> $ncl_logfile || ret_code=$?
        #-------------------------------------------------------------
        # Fatal errors catching mechanism
        # (needed for NCL versions <= 6.1.0)
        #-------------------------------------------------------------
        # - Grepping $ncl_logfile
        if [ ${ret_code:-0} != 0 ] || grep -Fq "fatal" $ncl_logfile ; then
        # if grep -Fq "fatal" $ncl_logfile; then
            info=$(caller 0)        # Calling function info
            OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
            info_array=( $info )
            IFS="$OLD_IFS"          # restoring original file separator

            funct_line=${info_array[0]}
            funct_name=${info_array[1]}
            funct_file=${info_array[2]}

            echo_message(){
                print_line 1
                echo " ncl error" # in $function_name"
                echo " (file $(trim $(basename $funct_file)), line ${funct_line})"
                print_line 1
                cat $ncl_logfile
        #        tail -n +6 $ncl_logfile
                print_line 1
            }
            echo_message 1>&2; exit 1
        else
            rm $ncl_logfile
        fi
        #-------------------------------------------------------------
    fi

    # #---------------------------------
    # # 1. NCL overflow Catching mechanism. see \ref devel Known Issues.
    # #---------------------------------
    # # - Checks if NCL process takes too long
    # #   notice that it increases overhead
    # #---------------------------------
    # ncl $ncl_script > $ncl_logfile &
    # ncl_pid=$! ; ncl_age=0 ; step=1
    # while [ -e /proc/${ncl_pid} -a /proc/${ncl_pid}/exe ]; do
    #     if [ "$ncl_age" -gt "60" ]; then
    #         echo "NCL is taking too long. Probably it overflowed"
    #         echo "Aborting process."
    #         exit 1
    #     fi
    #     (( ncl_age += $step )); sleep $step
    # done
    #--------------------------------------------------
    # # - Checking if NCL log grows uncontrollably (not working)
    #--------------------------------------------------
    # while [ -e /proc/${ncl_pid} -a /proc/${ncl_pid}/exe ]; do
    #     log_size=$(wc -l $ncl_logfile | cut -f1 -d' ')
    #   if [ "$log_size" -gt "100" ]; then
    #       echo "NCL overflow in $(calling_function)"
    #       exit 1
    #   fi
    #   sleep 1
    # done

    # #------------------------------------------------
    # # - or Checking NCL_abnormal_exit file existence
    # #------------------------------------------------
    # # requires adding next lines at the start/end
    # # of ncl scripts, which is rather polluting.
    # # system("touch NCL_abnormal_exit")
    # # system("rm NCL_abnormal_exit")
    # #------------------------------------------------
    # if [ -e NCL_abnormal_exit ]; then
    #   print_line 2
    #     echo " NCL error in $(calling_function)"
    #   print_line 2
    #   echo " ";
    #   tail -n +6 $ncl_logfile
    #   print_line 2
    #   exit 1
    # else
    #   rm $ncl_logfile
    # fi
}

#===================================================
#> @brief Produce nice horizontal pdf graphs
#> Somehow this is hard to get directly from NCL
#===================================================
pdf_ncl(){
    local figure_name=$1
    call_ncl ${figure_name}.ncl
    pdf90 "${figure_name}.pdf" &> /dev/null
    mv "${figure_name}-rotated90.pdf" "${figure_name}.pdf" > /dev/null
}

setup_modules(){
    case $(hostname) in
  tux04|tux21|calc02)
      #export MODULEPATH=/net/opt/system/modules/3.2.8/modulefiles
      #/net/opt/system/modules/3.2.8/bin/modulecmd bash load modules
      setup_modules_system
      ;;
  calc01|calc03|calc04)
      source /net/opt/system/modules/default/init/bash
      ;;
    esac
}

setup_modules_system(){
    MODULE_VERSION=3.2.8
    export MODULEPATH="/net/opt/system/modules/$MODULE_VERSION/modulefiles"

    case "$0" in
        -sh  |  sh|*/sh  )  modules_shell=sh   ;;
        -ksh | ksh|*/ksh )  modules_shell=ksh  ;;
        -zsh | zsh|*/zsh )  modules_shell=zsh  ;;
        -bash|bash|*/bash)  modules_shell=bash ;;
    esac

    module() { eval `/net/opt/system/modules/$MODULE_VERSION/bin/modulecmd bash $*`; }
    module load modules
}

remove_from_PATH(){
    local REMOVE="$1"
    WORK=":${PATH}:"
    WORK=${WORK/:$REMOVE:/:}
    WORK=${WORK%:}; WORK=${WORK#:}
    export PATH=$WORK
}

clone_code(){
    # Finding free temporary folder
#    TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-speedy_copy${count}"; }
    count=1 # Code copy number
    while [[ -d $(TMP_CODE_DIR) ]]; do (( count++ )); done

    create_code_copy "$(TMP_CODE_DIR)"
    cd "$(TMP_CODE_DIR)/code"
    source ./initialize_das.sh
}

#-----------------------------------------------------------------------
#> Copy code excluding git repository, doxydoc, disabled stuff and docs
#-----------------------------------------------------------------------
create_code_copy(){
    funct_opening 1

    output_dir=$1
    find .. \
        \( -type f -o -type l \) \
        -a \( ! -path "*bin*" \) \
        -a \( ! -path "*disabled*" \) \
        -a \( ! -path "*tmp*" \) \
        -a \( ! -path "*logs*" \) \
        -a \( ! -path "*.git*" \) \
        -a \( ! -path "*doxydoc*" \) \
        -a \( ! -path "*docs*" \) \
        -a \( ! -path "*.#*" \) \
        | while read file_path; do
        #echo $file_path 1>&2
        file_path__=${file_path:3} # removing first 2 characters
        file_dir="${file_path__%/*}"
        # echo "file_path  = $file_path"
        # echo "file_path__= $file_path__"
        # echo "file_dir   = $file_dir"
        mkdir -p "$output_dir/$file_dir"
        cp -fraL "$file_path" "$output_dir/$file_path__" # cp preserving attributes
    done
    echo " A copy of the code was created in"
    echo " $output_dir"

    funct_closing 1
}

#------------------------------------------------------
#> @brief Create new doxygen documentation
#------------------------------------------------------
create_doxygen_document(){
    funct_opening 3

    HTML_OUTPUT=$CODE_DIR/doxydoc_files
    HTML_LINK=$CODE_DIR/doxydoc.html
    FILE_PATTERNS=${FILE_PATTERNS:-'*.f *.f90 *.sh *.txt'}

    echo " - Removing old documentation"
    rm -f Doxyfile ${HTML_LINK}
    rm -fr ${HTML_OUTPUT}

    echo " - Generating Development Log"
    echo "/*! @page devellog Development Log" > git_log.txt
#    git log --pretty=oneline --abbrev-commit >> git_log.txt
    git log --abbrev-commit  >> git_log.txt || echo "no git repository"
    echo "*/" >> git_log.txt

    echo " - Creating new doxygen config file"
    doxygen -g > /dev/null

    echo " - Customizing it"
    
    if [[ -n ${PROJECT_NAME:-} ]]; then
	replace "PROJECT_NAME"      "PROJECT_NAME =  \"$PROJECT_NAME\""  Doxyfile
    fi

    replace "INPUT "                "INPUT        =  $CODE_DIR" Doxyfile
    replace "HTML_OUTPUT"           "HTML_OUTPUT  =  $HTML_OUTPUT" Doxyfile


    replace "FILE_PATTERNS" "FILE_PATTERNS = $FILE_PATTERNS" Doxyfile
    replace "OPTIMIZE_FOR_FORTRAN"  "OPTIMIZE_FOR_FORTRAN   = YES" Doxyfile
# Filter for bash scripts
    replace "FILTER_SOURCE_FILES"   "FILTER_SOURCE_FILES = YES"    Doxyfile
    replace "INPUT_FILTER" "INPUT_FILTER = \"sed -e 's|#>|///|' -e 's|#|//|' -e 's|\[ -d|\[ -dir|'\"" Doxyfile
    replace "RECURSIVE"             "RECURSIVE              = YES" Doxyfile

    set -f # Turning globbing off
    replace "EXCLUDE_PATTERNS" \
  "EXCLUDE_PATTERNS = */disabled/* */doxydoc_files/*"        Doxyfile
    set +f # Turning globbing on

    replace "SOURCE_BROWSER"        "SOURCE_BROWSER         = YES" Doxyfile
    replace "INLINE_SOURCES"        "INLINE_SOURCES         = YES" Doxyfile
    replace "GENERATE_LATEX"        "GENERATE_LATEX         = NO"  Doxyfile
    replace "EXTRACT_ALL"           "EXTRACT_ALL            = YES" Doxyfile
    replace "EXTRACT_PRIVATE"       "EXTRACT_PRIVATE        = YES" Doxyfile
    replace "EXTRACT_STATIC"        "EXTRACT_STATIC         = YES" Doxyfile
    replace "EXTRACT_LOCAL_METHODS" "EXTRACT_LOCAL_METHODS  = YES" Doxyfile
    replace "HAVE_DOT"              "HAVE_DOT               = YES" Doxyfile
    replace "CALL_GRAPH"            "CALL_GRAPH             = YES" Doxyfile
    replace "CALLER_GRAPH"          "CALLER_GRAPH           = YES" Doxyfile
    replace "HIDE_UNDOC_RELATIONS"  "HIDE_UNDOC_RELATIONS   =  NO" Doxyfile
    replace "IMAGE_PATH"            "IMAGE_PATH = \"$CODE_DIR/docs\""  Doxyfile
    replace "DOT_IMAGE_FORMAT"      "DOT_IMAGE_FORMAT       = jpg" Doxyfile
#    replace "UML_LOOK  "              "UML_LOOK            = YES"  Doxyfile
    replace "TEMPLATE_RELATIONS"    "TEMPLATE_RELATIONS     = YES" Doxyfile
    replace "MAX_DOT_GRAPH_DEPTH"   "MAX_DOT_GRAPH_DEPTH    =   3" Doxyfile
    replace "DOT_MULTI_TARGETS"     "DOT_MULTI_TARGETS      = YES" Doxyfile
    replace "STRIP_CODE_COMMENTS"   "STRIP_CODE_COMMENTS    =  NO" Doxyfile
    # Perhaps Bash inclusion can be done through extension mapping
    #replace "EXTENSION_MAPPING"     "EXTENSION_MAPPING  = .m=C++"  Doxyfile

    # replace "ENABLE_PREPROCESSING"  "ENABLE_PREPROCESSING   = YES" Doxyfile
    # replace "MACRO_EXPANSION"       "MACRO_EXPANSION        = YES" Doxyfile
    # replace "EXPAND_ONLY_PREDEF"    "EXPAND_ONLY_PREDEF     = YES" Doxyfile

    #QUIET                  = YES
    #WARNINGS               = NO
    #WARN_IF_UNDOCUMENTED   = NO
    #WARN_IF_DOC_ERROR      = NO

    echo " - Calling Doxygen"
    doxygen Doxyfile > /dev/null
# doxygen -u Doxyfile > /dev/null # update flag (not working)

    echo " - Creating relative symbolic links"
    ln -fs doxydoc_files/index.html ${HTML_LINK} # relative link for portability

    rm -f Doxyfile git_log.txt # Cleaning up

    funct_closing 3
}
