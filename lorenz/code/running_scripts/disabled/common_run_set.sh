#!/usr/bin/env bash

#=======================================================================
#> @brief Gather run set stat plots into a pdf document
#=======================================================================
run_set_report(){
    funct_opening 1
    run_set_dir=$1
    report_name=$2
    
    cd $run_set_dir

    open_tex_file "$report_name"

    find . -name "*.pdf" | sort | new_page "Overall "

    #find . -name "*Emean_?????_c*Tmean*.pdf" | sort | new_page "Overall Mean state"
    #find . -name "*Emean_?????_c*Tstdd*.pdf" | sort | new_page "Overall State Standard Deviation"
    #find . -name "*error*.pdf" | sort | new_page "Root Mean Square Error"
    #find . -name "*RMSSS*.pdf" | sort | new_page "Root Mean Square Skill Score"
    close_tex_file

    pdflatex ${doc_name}.tex > /dev/null
    #latex ${doc_name}.tex; dvipdf ${doc_name}.dvi
    #dvips ${doc_name}.dvi; rm *.dvi; ps2pdf ${doc_name}.ps
    rm *.aux;  rm *.log; #rm *.tex

    funct_closing 1
}


##=======================================================================
##> @brief Do statistical analysis of a run set
##=======================================================================
#run_set_stats(){
    #funct_opening 2

    #dataset_dir=$1;  cd $dataset_dir

    #read run_mode < config/run_mode.cfg
    #readarray -t run_names < config/run_names.cfg
    #read      dataset_kind < runs/${run_names[0]}/config/dataset_kind.cfg
    #read            n_comp < runs/${run_names[0]}/config/n_comp.cfg
    #read       dataset_dim < config/dataset_dim.cfg

    #analize_individual_runs

    #echo " Finding out parameter span"

    #case "$dataset_dim" in
        #1)  read par1_name < config/par1_name.cfg;
            #par_name[0]=$par1_name
            #;;
        #2)  read par1_name < config/par1_name.cfg
            #read par2_name < config/par2_name.cfg
            #par_name[0]=$par1_name
            #par_name[1]=$par2_name
            #;;
    #esac

    #gather_stats_into_lists

    #create_netcdfs

    #create_plots

    #create_pdf_report

##  needs to be more robust
##    save_hard_drive

    #funct_closing 2
#}

#analize_individual_runs(){
    #funct_opening 1

    #nprocess=0
    #for run_name in ${run_names[@]}; do

        #${dataset_kind}_stats.sh $dataset_dir/runs/$run_name \
            #> ${run_name}_stats.log &
        #pids[$nprocess]=$!
        #echo " - run $run_name [PID ${pids[$nprocess]}]"

        #(( nprocess+=1 ))
        #if [ $(( nprocess%cpus )) == 0 ]; then
            #for pid in ${pids[@]};do
                #wait $pid || error "Child process $pid crashed"
            #done
            #unset pids
        #fi
    #done

    ## wait for a possible incomplete process batch
    #for pid in ${pids[@]:-};do
        #wait $pid || error "Child process $pid crashed"
    #done
    #unset pids

    #rm *_stats.log

    #funct_closing 1
#}

#gather_stats_into_lists(){
    #funct_opening 1

    #rm -fr stats # cleaning previous lists
    #readarray -t scalars < \
        #<(cd $dataset_dir/runs/"${run_names[0]}"/stats; ls *.dat|cut -d'.' -f1)
    #for run_name in ${run_names[@]}; do
        ## - stats
        #for scalar in ${scalars[@]}; do
            #readarray -t scalar_val < $dataset_dir/runs/$run_name/stats/${scalar}.dat
            #echo "${scalar_val[@]}" >> ${scalar}.list
        #done
    #done
    #cd $dataset_dir; mkdir stats; mv *.list stats

    #funct_closing 1
#}

#create_netcdfs(){
    #funct_opening 1

    #cd $dataset_dir/stats

    #echo " Netcdf files created:"

    #for scalar in "${scalars[@]}"; do
        #case "$dataset_dim" in
            #1)  cp ../config/${par1_name}_value_pos.cfg ${par1_name}_value_pos.list
                #cp ../config/${par1_name}_values.cfg    ${par1_name}_values.list
                #create_netcdf_scalar_vs_par1 $scalar $par1_name
                #;;
            #2)  cp ../config/${par1_name}_value_pos.cfg ${par1_name}_value_pos.list
                #cp ../config/${par1_name}_values.cfg    ${par1_name}_values.list
                #cp ../config/${par2_name}_value_pos.cfg ${par2_name}_value_pos.list
                #cp ../config/${par2_name}_values.cfg    ${par2_name}_values.list
                #create_netcdf_scalar_vs_par1_par2 $scalar $par1_name $par2_name
                #;;
        #esac
        #echo " - ${scalar}.nc"
    #done
##    ls *.nc | xargs -I % mv % ../

    #funct_closing 1
#}

#create_plots(){
    #funct_opening 1
    
    #cd $dataset_dir
    #rm -fr plots; mkdir plots
    #cd stats

    #for scalar in "${scalars[@]}"; do
        #case "$dataset_dim" in
            #1)  run_set_1D_stat_plot ${scalar}
                #;;
            #2)  run_set_2D_stat_plot ${scalar}
                ## if [[ "$scalar" = *RMSSS* ]]; then
                ##     contour_plot $scalar $scalar "-1.0" "1.0"
                ## else
                ##     contour_plot $scalar $scalar
                ## fi
                #;;
        #esac
    #done

##    ls *.nc | xargs -I % mv % ../
    #mv *.pdf ../plots/ || echo "No pdfs to move"

    #funct_closing 1
#}


#save_hard_drive(){
    #funct_opening 1

    #cd $dataset_dir
    #storage_saving=1
    #if [[ $storage_saving -ge 1 ]]; then
        ## if [ $run_mode == "free" ];then
        ##     find . -wholename *nature_Insta_all.nc | xargs rm
        ## fi
        #find . -name *nature_obs_clean_Insta_4b.grd | xargs rm
        #find . -name *.list | xargs rm
    #fi
    #if [[ $storage_saving -ge 2 ]]; then
        #find . -wholename */runs/*.nc | xargs rm
    #fi

    #funct_closing 1
#}

#create_netcdf_scalar_vs_par1(){
    #local scalar_name=$1
    #local   par1_name=$2

    #rm -f ${scalar_name}.nc
    #cat > ${scalar_name}.ncl <<_EOF_
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
#begin
#; - Read in data
#scalar_list = readAsciiTable("${scalar_name}.list",$n_comp,"float",0)
#par1_pos    = readAsciiTable("${par1_name}_value_pos.list",1,"integer",0)
#par1_values = readAsciiTable(   "${par1_name}_values.list",1,"float",0)

#list_length = max(dimsizes(scalar_list))
#par1_span_length = max(dimsizes(par1_values))

#scalar_nc   = addfile("${scalar_name}.nc","c")
#scalar = new((/par1_span_length,1/),float)

#do icomp = 1, $n_comp
   #var_name = "comp" + sprinti("%1.1i",icomp)

   #do i = 0, list_length-1
      #scalar(par1_pos(i,0),0) = scalar_list(i,icomp-1)
   #end do

#; - Add attributes
   #scalar!0                      = "${par1_name}"
   #scalar&${par1_name}           =    par1_values(:,0)
   #scalar&${par1_name}@long_name = "${par1_name}"

   #scalar_nc->\$var_name\$ = scalar
#end do

#end
#exit
#_EOF_
    #call_ncl ${scalar_name}.ncl
    #rm ${scalar_name}.list
    #rm ${scalar_name}.ncl
#}

#create_netcdf_scalar_vs_par1_par2(){
    #local scalar_name=$1
    #local   par1_name=$2
    #local   par2_name=$3

    #rm -f ${scalar_name}.nc
    #cat > ${scalar_name}.ncl <<_EOF_
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
#load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
#begin
#; - Read in data
#scalar_list = readAsciiTable("${scalar_name}.list",$n_comp,"float",0)
#par1_pos    = readAsciiTable("${par1_name}_value_pos.list",1,"integer",0)
#par1_values = readAsciiTable(   "${par1_name}_values.list",1,"float",0)
#par2_pos    = readAsciiTable("${par2_name}_value_pos.list",1,"integer",0)
#par2_values = readAsciiTable(   "${par2_name}_values.list",1,"float",0)

#list_length = max(dimsizes(scalar_list))
#par1_span_length = max(dimsizes(par1_values))
#par2_span_length = max(dimsizes(par2_values))

#scalar_nc   = addfile("${scalar_name}.nc","c")
#scalar = new((/par1_span_length,par2_span_length/),float)

#do icomp = 1, $n_comp
   #var_name = "comp" + sprinti("%1.1i",icomp)

   #do i = 0, list_length-1
      #scalar(par1_pos(i,0),par2_pos(i,0)) = scalar_list(i,icomp-1)
   #end do

#; - Add attributes
   #scalar!0                      = "${par1_name}"
   #scalar&${par1_name}           =    par1_values(:,0)
   #scalar&${par1_name}@long_name = "${par1_name}"
   #scalar!1                      = "${par2_name}"
   #scalar&${par2_name}           =    par2_values(:,0)
   #scalar&${par2_name}@long_name = "${par2_name}"

   #scalar_nc->\$var_name\$ = scalar
#end do

#end
#exit
#_EOF_
    #call_ncl ${scalar_name}.ncl
    #rm ${scalar_name}.ncl
#}

#set -o errexit # non-zero return values make the script halt
#set -o nounset # Unset variable expansion gives error
#source ${COM_DAS_DIR}/common_das_tools.sh
#source ${COM_DAS_DIR}/common_plots.sh
#source ${COM_DAS_DIR}/common_run_set.sh
#run_set_stats $@
#exit $?
