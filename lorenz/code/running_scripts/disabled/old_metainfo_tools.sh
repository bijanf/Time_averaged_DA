#set_par_values_file(){
    #local    par_name=$1
    #local    par_span=$2
    #local name_prefix=$3

    #case $par_name in
        ## - Real valued parameters (must use numpy syntax)
        #cycle_length|F_ampl|SNR|xlocal|link)
            #echo_real_par_values $par_span > ${name_prefix}_values.cfg
            #;;
        ## - Integer valued parameters
        #cycles)
            #par_span_expanded=$(eval echo $par_span)
            #printf '%s\n' ${par_span_expanded[@]} > ${name_prefix}_values.cfg
            #;;
        ## - string valued parameters (must use python list syntax)
        #update_mode|obs_operator)
            #echo_string_par_values $par_span > ${name_prefix}_values.cfg
            #;;
        #*) error "Unsupported parameter $par_name";;
    #esac

    #readarray -t ${name_prefix}_values < ${name_prefix}_values.cfg
    #eval "${name_prefix}_span_length=\${#${name_prefix}_values[@]}"
    #set_par_file ${name_prefix}_span_length
#}

# echo_real_par_values(){
#     par_span=$1 #( numpy array)
#     ( cat | python )<<_EOF_
# from scipy import arange
# import numpy
# par_values = numpy.$par_span
# for par_value in par_values:
#     par_value_str = "%06.3f" % par_value
#     print par_value_str
# _EOF_
# }

# echo_string_par_values(){
#     par_span=$1 #(python list)
#     ( cat | python )<<_EOF_
# par_values = $par_span
# for par_value in par_values:
#     print par_value
# _EOF_
# }


#create_scalar_netcdf(){
    #scalar=$1
    #declare -a opts
    #opts+=($(printf '%s=\"%s\"' scalar_name $scalar))
    #opts+=($(printf '%s=\"%s\"' cfg_dir     $dataset_dir/config))
    ## my_ncl ${opts[@]} $DAS_DIR/create_netcdf_from_list.ncl
    #my_ncl -x --logfile="${scalar}.ncl_log" \
        #${opts[@]} $DAS_DIR/create_netcdf_scalar.ncl
    #unset opts
#}

#create_scalar_netcdf(){
    #scalar=$1
    #create_scalar_list   $scalar
    #declare -a opts
    #opts+=($(printf '%s=\"%s\"' scalar_name $scalar))
    #opts+=($(printf '%s=\"%s\"' cfg_dir     $dataset_dir/config))
    ## my_ncl ${opts[@]} $DAS_DIR/create_netcdf_from_list.ncl
    #my_ncl --logfile="${scalar}.ncl_log" \
        #${opts[@]} $DAS_DIR/create_netcdf_from_list.ncl
    #unset opts
    #rm ${scalar}.list
#}

#create_scalar_list(){
    #local scalar=$1
    #for run_name in "${run_names[@]}"; do
                                #scalar_file="../runs/$run_name/stats/${scalar}.dat"
        #if [[ -e "$scalar_file" ]]; then
            #readarray -t scalar_val<"$scalar_file"
        #else
            #for (( icomp=0; icomp < n_comp; icomp++ )); do
                #scalar_val[icomp]="-9.99e+33" # CDO float NaN _FillValue
                ##scalar_val[icomp]="9.96921e+36" # NCL float NaN _FillValue
            #done
        #fi
        ##if [[ -e "../runs/$run_name/config/run_crashed.cfg" ]]; then
            ##for (( icomp=0; icomp < n_comp; icomp++ )); do
                ##scalar_val[icomp]="-9.99e+33"   # CDO float NaN _FillValue
                ###scalar_val[icomp]="9.96921e+36" # NCL float NaN _FillValue
            ##done
        ##else
            ##readarray -t scalar_val<"../runs/$run_name/stats/${scalar}.dat"
        ##fi
        #echo "${scalar_val[@]}" >> ${scalar}.list
    #done
#}
