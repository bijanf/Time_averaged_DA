#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)

model_number=$1
task=${2:-docu}
what=${3:-runs}
arch_dir="/daten/model/acevedo/das_archive/batch2"


model_id[0]="L63"
model_id[1]="L63_2s/uncoupled"
model_id[2]="L63_2s/extratropical_Ocen_atm_strong_coupling"
model_id[3]="L96_2s_mix/link-0.0d0"
model_id[4]="L96_2s_mix/link-0.2d0"
model_id[5]="L96_2s_mix/link-0.2d0__obs_operator-vsl0"

model_dir="$arch_dir/${model_id[$model_number]}"


case ${what} in
    runs)
        case ${task} in
            docu)
                find "$model_dir/single_runs" -maxdepth 2 -path "*assi*" | \
                    xargs -I{} run.sh --task=docu {}
                ;;
            show)
                find "$model_dir/single_runs" -maxdepth 2 -path "*.pdf"| \
                    xargs #okular
                ;;
        esac
        ;;
    sets)
        case ${task} in
            docu)
                find $model_dir -maxdepth 2 -path "*run_set_bunch*" | \
                    xargs -I{} run_set_bunch.sh --task=docu {}
                ;;
            show)
                find $model_dir -maxdepth 3 -path "*run_set_bunch*.pdf" | \
                    xargs okular
                ;;
        esac
        ;;
esac
