#!/usr/bin/env bash

set_cfg_file(){
    # [[ $# -eq 1 ]] || error "Usage: set_par_file par_name"
    local par_name=$1
    local config_dir=${2:-"./config"}
    local bash_line="echo \"\${$par_name}\" > $config_dir/${par_name}.cfg"
#    echo $bash_line
    eval $bash_line
    local par_value; read par_value < "$config_dir/${par_name}.cfg"
    printf " %-22s = %s\n" "$par_name" "$par_value"
}

get_config_file(){
    local file_path=$1
    [[ -e "$file_path" ]] || error "Non existent config file $file_path"

    local var_name="$(barename $file_path)"
    local field_value=$(cat $file_path)
    unset cfg[$var_name]
    cfg[$var_name]=$(trim $field_value)

    # cfg[$var_name]=$(cat $file_path)
}

add_to_cfg(){
    # var_names=$@
    for var_name in $@;do
        eval "cfg[$var_name]=\$(trim \$$var_name)"
        # printf " %-22s = %s\n" "$var_name" "${cfg[$var_name]}"
    done
}

print_config(){
    local cfg_dir=${1:-"."}
    format=" %24s = %20s\n"
    cfg_files=($(find "$cfg_dir" -name "*.cfg"))

    cfg_names=($(barename ${cfg_files[@]}))
    cfg_names=($(order_array cfg_names))
    for cfg_name in "${cfg_names[@]}"; do
        cfg_value=$(cat "$cfg_dir/${cfg_name}.cfg")
        cfg_value=$(trim $cfg_value)
        cfg_value=${cfg_value:0:35}
        # read cfg_value < "$cfg_dir/${cfg_name}.cfg"
        printf "$format" "$cfg_name" "$cfg_value"
    done
}

par_id(){
    local par_names=$@
    local string=""
    for par_name in ${par_names[@]}; do
            # echo "par_value=\$${par_name}"
        eval "par_value=\$${par_name}"
        string="${string}__${par_name}_${par_value}"
    done
    string="${string:2:${#string}}" # removing first __
    echo "$string"
}

# get_config_file(){
#     local file_path=$1
#     [[ -e "$file_path" ]] || error "Non existent config file $file_path"

#     # var_name=$(basename "$file_path")
#     # var_name=${file_path##*/}; var_name=${var_name%.*}
#     var_name="$(barename $file_path)"

#     a=0
#     while read line; do
#         config_var[$a]="$(trim $line)"
#         a=$(($a+1));
#     done < "$file_path"
#     bash_line1="unset $var_name"
#     bash_line2="$var_name=(\${config_var[@]:-})"
# #    echo $bash_line1; echo $bash_line2;
#     eval $bash_line1; eval $bash_line2;

#     unset config_var
# }

get_config(){
    local cfg_dir=${1:-"./config"}
    cfg=()
    # if [[ $(basename $cfg_dir) != "config" ]];then
    #     error "Non-existent config dir $cfg_dir"
    # fi
    cfg_files=($(find "$cfg_dir" -name "*.cfg"))
    for cfg_file in "${cfg_files[@]}"; do
        # echo "$cfg_file"
        get_config_file "$cfg_file"
    done
}

store_cfg(){
    local cfg_dir=${1:-"./config"}
    for field in ${!cfg[@]}; do
        echo "${cfg[$field]}" >> $cfg_dir/${field}.cfg
    done
}

print_cfg(){
    format=" %24s = %20s\n"
    for field in ${!cfg[@]}; do
        printf "$format" "$field" "${cfg[$field]}"
    done
}

set_par_values_file(){
    local    par_name=$1; shift 1
    local    par_span="$@"
    local config_dir=${config_dir:-"./config"}

    cat > set_par_values_file.ncl <<EOF
par_values  = ${par_span[@]}
par_type    = typeof(par_values)
span_length = max(dimsizes(par_values))

par_string = new(span_length,"string")
if(par_type.eq."integer") then
    par_string = sprinti("%05i",par_values)
end if
if(par_type.eq."float") then
    par_string = sprintf("%06.3f",par_values)
end if
if(par_type.eq."string") then
    par_string = par_values
end if

asciiwrite ("$config_dir/${par_name}_values.cfg"     ,par_string)
asciiwrite ("$config_dir/${par_name}_type.cfg"       ,par_type)
asciiwrite ("$config_dir/${par_name}_span_length.cfg",span_length)

exit
EOF
    my_ncl set_par_values_file.ncl
    rm set_par_values_file.ncl
}

# set_par_values_file(){
#     local    par_name=$1; shift 1
#     local    par_span="$@"
#     local config_dir=${config_dir:-"./config"}

#     cat > set_par_values_file.ncl <<EOF
# par_values  = ${par_span[@]}
# par_type    = typeof(par_values)
# span_length = max(dimsizes(par_values))

# if(par_type.eq."integer") then
#     format = "%05i"
# end if
# if(par_type.eq."float") then
#     format = "%06.3f"
# end if
# if(par_type.eq."string") then
#     format = "%s"
# end if

# write_table("$config_dir/${par_name}_values.cfg","w",[/par_values/],format)
# asciiwrite ("$config_dir/${par_name}_type.cfg"        ,par_type)
# asciiwrite ("$config_dir/${par_name}_span_length.cfg" ,span_length)

# exit
# EOF
#     my_ncl set_par_values_file.ncl
#     rm set_par_values_file.ncl
# }

parse_dataset_kind(){
    # get_config_file "$dataset_dir/config/dataset_kind.cfg"
    if [[ $dataset_kind != ${cfg[dataset_kind]} ]];then
        echo "$dataset_kind != ${cfg[dataset_kind]}" 1>&2
        error "dataset_kind mismatch"
    fi
}

#-----------------------------------------
# labeling function for dataset reports
#-----------------------------------------

label(){
    string="$1"
    criterion=$2
    error_message(){ error "No $criterion label for $string";}
    case $criterion in
        run|Ephase|Tkind|update_mode|infl_mode|obs_operator|speedy_var|Quantity)
            ${criterion}_l "$string"
            ;;
        par)
            echo "$(long_name $string)"
            ;;
        loc_radius|S|tao|cycle_length|comp_localization)
            echo "\url{$string}"
            ;;
        *)
            echo "\url{$string}";
            echo "Warning: Unknown label criterion $criterion" 1>&2
            ;;
    esac
}

run_l(){
    case "$1" in
        *free*prior ) echo "Free Forecast";;
        *free*      ) echo "Free Ensemble";;
        *assi*prior*) echo "D.A. Forecast";;
        *assi*postr*) echo "D.A. Analysis";;
        *assi*      ) echo "D.A. Ensemble";;
        *nature*    ) echo "Nature Run";;
        *           ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

Ephase_l(){
    case "$1" in
        *prior* ) echo "Forecast";;
        *postr* ) echo "Analysis";;
        *nature*) echo "Nature";;
        *       ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

Tkind_l(){
    case "$1" in
        *Insta*) echo "Instantaneous";;
        *Taver*) echo "Time-Averaged";;
        *Tanom*) echo "T-A. Anomaly";;
        *      ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

update_mode_l(){
    case "$1" in
        *Insta*) echo "Instantaneous";;
        *Hakim*) echo "Time-Averaged";;
        *Augm0*) echo "Time-Augmented";;
        *Augm1*) echo "Time-Augmented";;
        *Augm2*) echo "TA + Tanomaly";;
        *Augm3*) echo "TA + Last Tanom.";;
        *Augm4*) echo "TA + Last Insta.";;
        *      ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

infl_mode_l(){
    case "$1" in
        *step_*) echo "Infl. every step";;
        *cycle*) echo "Infl. every cycle";;
        *      ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

obs_operator_l(){
    case "$1" in
        *iden*) echo "Identity Obs";;
        *addi*) echo "Addition";;
        *plus*) echo "Linear sup.";;
        *vsl0*) echo "VS-lite";;
        *vsl1*) echo "VSL-smooth";;
        *) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
#        *     ) echo "\url{$1}"; error "Unlabelable string $1";;
    esac
}

speedy_var_l(){
    case "$1" in
        u ) echo "U-wind";;
        v ) echo "V-wind";;
        t ) echo "Temperature";;
        q ) echo "Humidity";;
        ps) echo "Surf. pressure";;
        * ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

Quantity_l(){
    case "$1" in
        *Emean_Tstdd*reduc*) echo "State Std. Deviation Reduction";;
        *Emean_Tstdd*      ) echo "State Std. Deviation";;
        *error*reducRefMin*) echo "RMSE Reduction RefMin";;
        *error*reduc*      ) echo "RMSE Reduction RefFree";;
        *error*incre*      ) echo "RMSE Increase RefAdd";;
        *obs_error_stdd*   ) echo "Obs. Error Std. Deviation";;
        *error*            ) echo "RMSE";;
        *Esprd*reduc*      ) echo "Spread Reduction";;
        *Esprd*            ) echo "Spread";;
        *nature_Tstdd*     ) echo "State Std. Deviation";;
        # *Tstdd*reduc*) echo "Std. Deviation Reduction";;
        # *Tstdd*      ) echo "Standard Deviation";;
        # *error*reduc*) echo "Error Reduction";;
        # *Tvar*       ) echo "Variance";;
        # *Tvar*reduc* ) echo "Variance Reduction";;
        # *Tvar*       ) echo "Variance";;
        # *error*      ) echo "Error";;
        # *Tstdd_Fmean*) echo "std. deviation.";;
        * ) echo "\url{$1}"; echo "Unlabelable string $1" 1>&2;;
    esac
}

long_name(){
    case "$1" in
        *cycle_length*) echo "Observation period";;
        *cycles*      ) echo "Assimilation cycles";;
        *Taver_length*) echo "Time average length";;
        *infl_enkf*   ) echo "Inflation factor";;
        *infl_mode*   ) echo "Inflation mode";;
        *SNR*         ) echo "SNR";;
        update_mode   ) echo "Updated State"  ;;
        loc_radius    ) echo "Localization radius"  ;;
        obs_operator  ) echo "Observation Operator" ;;
        sat_level     ) echo "Resp. funct. thresholding" ;;
        speedy_var    ) echo "Atmospheric variable" ;;
        comp_localization) echo "Component wise localization" ;;
        *             ) echo "\url{$1}"; echo "Undefined longname for parameter $1" 1>&2;;
    esac
}

#========================================================
#> @brief Display all Overall statistics files (*.dat)
#> present in the current folder
#========================================================
print_statistic(){
    local stat_file=$1
    [[ -f $stat_file ]] || error "Non-existent stat_file $stat_file"

    readarray -t stat < $stat_file
        # stat_name=${stat_file%.*}; stat_name=${stat_name##*/}
    stat_name=$(barename $stat_file)
    if [[ "$(trim ${stat[0]})" == "-9.99e+33" ]]; then
        case ${cfg[n_comp]} in
            1) format=" %40s     NaN \n"       ;;
            2) format=" %40s     NaN   NaN \n" ;;
        esac
        printf "$format" "$stat_name"
    else
        case ${cfg[n_comp]} in
            1)  format=" %40s %+7.2e\n"    ;;
            2)  format=" %40s %7.3f %7.3f\n"   ;;
        esac
        printf "$format" "$stat_name" "${stat[@]}"
    fi
}

print_statistics(){
    stats_dir=${stats_dir:-"."}

    case ${cfg[n_comp]} in
        1)header=" STATISTIC                                | Value |"      ;;
        2)header=" STATISTIC                                | Comp1 | Comp2";;
    esac

    print_line 1
    echo "$header"
    print_line 1
    ls ${stats_dir}/*Insta.dat | sort -r | while read stat_name; do
        print_statistic $stat_name
    done
    print_line 1
    ls ${stats_dir}/*Taver.dat | sort -r | while read stat_name; do
        print_statistic $stat_name
    done
    print_line 1
    ls ${stats_dir}/*Tanom.dat | sort -r | while read stat_name; do
        print_statistic $stat_name
    done
    print_line 1
    ls ${stats_dir}/covar_nature_*__obs_clean_Taver.dat 2>/dev/null \
        | sort -r | while read stat_name; do
        print_statistic $stat_name
    done
    # ls ${stats_dir}/corr_nature_*__obs_clean_Taver.dat 2>/dev/null \
    # | sort -r | while read stat_name; do
    #     print_statistic $stat_name
    # done
    #[[ -f corr_nature_Tanom__obs_clean_Taver.dat ]] && \
      #print_statistic corr_nature_Tanom__obs_clean_Taver.dat
    print_line 1

}

create_line_plot(){

    opts+=($(printf '%s=\"%s\"' plot_name "$(plot_name)"))
    opts+=(n_comp=${cfg[n_comp]});
    echo ${opts[@]} > $(plot_name).opts
    unset opts

    echo "$(plot_title)" > $(plot_name)_title.txt
    for file in ${line_files[@]}; do
        echo "$file"         >> $(plot_name)_files.txt
        echo "$(line_label)" >> $(plot_name)_labels.txt
    done
    my_ncl --logfile="$(plot_name).ncl_log" \
        $(cat $(plot_name).opts) line_plot.ncl

    unset    line_files
    unset -f line_label
    unset -f plot_title
    unset -f plot_name
}

create_surface_plot(){
    local scalar="$1"
    declare -a opts
    opts+=($(printf '%s=\"%s\"' plot_name $scalar))
    opts+=($(printf '%s=\"%s\"' nc_path ../stats/${scalar}.nc))
    opts+=(n_comp=${cfg[n_comp]}); opts+=(raster_mode=1)
    # opts+=($(printf '%s=\"%s\"' contour_lines off))
    case "$scalar" in
        *reduc*)
            opts+=(min_level=-100.0); opts+=(max_level=100.0)
            opts+=(levels=20)       ; opts+=(reverse_colormap=1)
            # opts+=(spacing=10.0) ; opts+=(reverse_colormap=1)
            # opts+=($(printf '%s=\"%s\"' color_map GMT_hot))
            # opts+=($(printf '%s=\"%s\"' color_map MPL_BuGn))
            # opts+=($(printf '%s=\"%s\"' color_map MPL_YlGn))
            #opts+=($(printf '%s=\"%s\"' color_map matlab_jet))
            opts+=($(printf '%s=\"%s\"' color_map MPL_jet))
            ;;
        # *Tstdd*|*Esprd*)
        *Esprd*)
            opts+=(min_level=0.0); opts+=(max_level=10.0)
            opts+=(levels=20)    ; #opts+=(reverse_colormap=1)
            opts+=($(printf '%s=\"%s\"' color_map MPL_YlGn))
            ;;
        *Tstdd*)
            opts+=(levels=20)    ; #opts+=(reverse_colormap=1)
            opts+=($(printf '%s=\"%s\"' color_map MPL_YlGn))
            ;;
        # *error*Tstdd_Fmean*)
        #     opts+=(min_level=0.0); opts+=(max_level=10.0)
        #     opts+=(spacing=1.0) ; #opts+=(reverse_colormap=1)
        #     # opts+=($(printf '%s=\"%s\"' color_map GMT_hot))
        #     # opts+=($(printf '%s=\"%s\"' color_map MPL_BuGn))
        #     # opts+=($(printf '%s=\"%s\"' color_map MPL_YlGn))
        #     ;;
    esac
    echo ${opts[@]} > ${scalar}.opts
    unset opts

    my_ncl --logfile="${scalar}.ncl_log" $(cat ${scalar}.opts) surface_plot.ncl
}
