#!/usr/bin/env bash
#set -xv
ensemble_assi_run_vs_par(){
    funct_opening 3
    dataset_dir=$1
    ens_free_dir=$2
    par_name=$3
    par_span=$4

    echo "==========================================="
    echo "TADA exp. sensitivity study with respect to"
    echo "parameter $par_name"
    echo "==========================================="

    create_archive_dir $dataset_dir || return 0
    cd $dataset_dir

    par_values=$(eval echo $par_span)
    local par_index=0
    for par_value in ${par_values[@]}; do
        (( par_index += 1 ))
        par_ind=$(printf '%04d' ${par_index})
        echo "$par_name -> $par_value"
        echo "$par_value">${par_ind}_${par_name}.dat

        (
            eval "export ${par_name}=${par_value}"
            export detail=1
            export   plot="off"
            { time -p ens_assi_run.sh $dataset_dir/$par_ind $ens_free_dir; } &> ${par_ind}_report.txt
        )
        cd $par_ind
        # Store Tmean stats
        ls *Tmean*.dat| xargs -I % mv % ../${par_ind}_%
        cd ..
        rm -r  $par_ind
    done
    par_index_max=$par_ind

    # Gather results
    # Merge individual exp stats into total files
    for pattern in {prior,postr}_{Insta,Taver}_{Esprd,Ermse}_Tmean $par_name; do
        (ls ????_${pattern}.dat | xargs cat) > "${pattern}_list.dat"
    done
    mkdir reports; ls ????_report* | xargs -I {} mv {} reports/

    rm ????_*

    echo $par_index_max > par_index_max.dat
    echo $par_name > par_name.cfg
    echo $par_span > par_span.cfg
    echo "$FUNCNAME" > dataset_kind.cfg
    mkdir config; mv *.cfg config

    [ $plot == 'on' ] && das_plot.sh $dataset_dir

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
#source   ${MODEL_DIR}/model_das_tools.sh

ensemble_assi_run_vs_par $@
exit $?