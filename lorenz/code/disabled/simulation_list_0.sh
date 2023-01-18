#!/usr/bin/env bash
#===================================================================
#> @page sim_list_0 Simulation batch 0
#===================================================================
simulation_list(){
    [[ $# -eq 1 ]] || error "Incorrect argument #"
    simulation_id=$1 # Simulation identifier

    echo " Launching simulation $simulation_id"

    case $simulation_id in
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_10: Noise level study (done)
        #===============================================================
        "2013_11_10")
            noise_level_study(){
                run_free_set_dir="$STORE/$model/run_free_set/link${link}_cycle_length"
                run_assi_set_dir="$STORE/$model/noise_level_study/update_mode_${update_mode}"
                yes | run_set.sh $run_assi_set_dir assi $run_free_set_dir \
                    "SNR" "linspace(0.5,5,8)"
                das_stats.sh $run_assi_set_dir
            }
            model="L96_2s_mix"; source $CODE_DIR/$model/default_config.sh
            das_make_all.sh $model
            export link="0.1d0"
            export update_mode="Hakim"; noise_level_study
            export update_mode="Augm1"; noise_level_study
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_08: Coupling influence for L96_2s_mix model (done)
        #===============================================================
        "2013_11_08")
            coupling_study(){
                run_free_set_dir="$STORE/$model/run_free_set/link_&_cycle_length"
                run_assi_set_dir="$STORE/$model/coupling_study/SNR_${SNR}/update_mode_${update_mode}"
                yes | run_set.sh $run_assi_set_dir assi $run_free_set_dir
                das_stats.sh $run_assi_set_dir
            }
            model="L96_2s_mix"; source $CODE_DIR/$model/default_config.sh
            das_make_all.sh $model

            export SNR="2.0"; export update_mode="Hakim"; coupling_study
            export SNR="2.0"; export update_mode="Augm1"; coupling_study
            export SNR="5.0"; export update_mode="Hakim"; coupling_study
            export SNR="5.0"; export update_mode="Augm1"; coupling_study
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_07: Filter tuning for L96_2s_mix model
        #>   - Best xlocal value for Hakim update_mode and obs every 2
        #>     gridpoints was exactly 2
        #>   - Augm1 assi run crashed!!!! resend it
        #===============================================================
        "2013_11_07")
            filter_tuning(){
                run_free_set_dir="$STORE/$model/run_free_set/CL_span_arange(1,42,5)*0.01"
                run_assi_set_dir="$STORE/$model/filter_tunning/run_assi_set/update_mode_${update_mode}"
                yes | run_set.sh $(run_assi_set_dir) assi $run_free_set_dir \
                    "xlocal" "linspace(1,8,8)"
                das_stats.sh $(run_assi_set_dir)
            }
            model="L96_2s_mix"; source $CODE_DIR/$model/default_config.sh
            das_make_all.sh $model
            # export update_mode="Hakim"; filter_tuning
            # !!!! next one crashed !!!
            export update_mode="Augm1"; filter_tuning
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_06: Run free sets for L96_2s_mix model (done)
        #===============================================================
        "2013_11_06")
            model="L96_2s_mix"; source $CODE_DIR/$model/default_config.sh
            das_make_all.sh $model

            export link="0.1d0"
            run_free_set_dir="$STORE/$model/run_free_set/link${link}_cycle_length"
            run_set.sh $run_free_set_dir free none \
                "cycle_length" "arange(1,42,5)*$dt"
            das_stats.sh $run_free_set_dir

            # run_free_set_dir="$STORE/$model/run_free_set/link_&_cycle_length"
            # run_set.sh $run_free_set_dir free none "link" "linspace(0,0.2,8)"\
            #     "cycle_length" "arange(1,42,5)*$dt"
            # das_stats.sh $run_free_set_dir
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_05: Filter tuning for L96_1s model
        #===============================================================
        "2013_11_05")
            model="L96_1s_pf"
            study_dir=$STORE/$model/filter_tunning
            run_free_set_dir=$study_dir/run_free_set
            run_assi_set_dir(){
                echo $study_dir/run_assi_set/update_mode_${update_mode};}

            das_make_all.sh $model
            source $CODE_DIR/$model/default_config.sh

            run_set.sh $run_free_set_dir free none \
                "cycle_length" "arange(1,42,4)*$dt"
            das_stats.sh $run_free_set_dir

            export update_mode="Hakim"
            run_set.sh $(run_assi_set_dir) assi $run_free_set_dir \
                "xlocal" "linspace(1,8,8)"
            das_stats.sh $(run_assi_set_dir) ;

            export update_mode="Augm1"
            run_set.sh $(run_assi_set_dir) assi $run_free_set_dir \
                "xlocal" "linspace(1,8,8)"
            das_stats.sh $(run_assi_set_dir) ;
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_11_01: Periodic forcing influence study in L96_1s_pf model
        #> - No aparent filter performance change due to periodic forcing
        #===============================================================
        "2013_11_04")
            F_ampl_study(){
                study_dir=$STORE/$model/F_ampl_study/CL_${CL_span}/F_tau${F_tau}
                run_free_set_dir=$study_dir/run_free_set
                run_assi_set_dir=$study_dir/run_assi_set
                run_set.sh $run_free_set_dir free none "F_ampl" "linspace(0,3,8)" \
                    "cycle_length" $CL_span
                run_set.sh $run_assi_set_dir assi $run_free_set_dir
                das_stats.sh $run_free_set_dir
                das_stats.sh $run_assi_set_dir ; }

            F_tau_study(){
                ( export F_tau="0.2"; F_ampl_study )
                ( export F_tau="0.5"; F_ampl_study )
                ( export F_tau="1.0"; F_ampl_study ) ; }

            local model="L96_1s_pf"
            das_make_all.sh $model
            source $CODE_DIR/$model/default_config.sh
            ( CL_span="arange(1,42,4)*$dt"   ; F_tau_study )
            ( CL_span="arange(10,110,10)*$dt"; F_tau_study )
            ;;
        #===============================================================
        #> @page sim_list_0
        #> 2013_10_19: Preliminar set of exp with Lorenz96_1s_pf model.
        #===============================================================
        "2013_10_25")
            local model="lorenz96_1s_pf"
            das_make_all.sh $model

            first_runs(){
                sampling_dir()       { echo $ARCH_DIR/L96_F_ampl${F_ampl}/sampling_1; }
                nature_vs_CL_dir()   { echo $ARCH_DIR/L96_F_ampl${F_ampl}/nature_vs_CL_1; }
                ens_free_vs_CL_dir() { echo $ARCH_DIR/L96_F_ampl${F_ampl}/ens_free_vs_CL_1; }
                ens_assi_vs_CL_dir() { echo $ARCH_DIR/L96_F_ampl${F_ampl}/ens_assi_vs_CL_$update_mode; }

                sampling_run.sh $(sampling_dir)
                nature_vs_cycle_length.sh $(nature_vs_CL_dir) $(sampling_dir) "arange(1,402,20)*$dt"
                # ens_free_vs_cycle_length.sh $(ens_free_vs_CL_dir) $(nature_vs_CL_dir)
                # export update_mode='Hakim'
                # ens_assi_vs_cycle_length.sh $(ens_assi_vs_CL_dir) $(ens_free_vs_CL_dir)
                # export update_mode='Augm1'
                # ens_assi_vs_cycle_length.sh $(ens_assi_vs_CL_dir) $(ens_free_vs_CL_dir)
            }

            source $CODE_DIR/$model/model_default_config.sh 1
            export F_ampl="0.0d0"; first_runs
            # export F_ampl="0.5d0"; first_runs
            # export F_ampl="1.0d0"; first_runs
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl0.0d0/ens_assi_vs_CL_Taver
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl0.0d0/ens_assi_vs_CL_Augm1
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl0.5d0/ens_assi_vs_CL_Taver
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl0.5d0/ens_assi_vs_CL_Augm1
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl1.0d0/ens_assi_vs_CL_Taver
            # das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl1.0d0/ens_assi_vs_CL_Augm1
            das_plot.sh /home/jowa/Desktop/das/data/L96_F_ampl1.0d0/ens_free_vs_CL_1/cl0100
            ;;

        *)  echo " No simulation with id $simulation_id"
            ;;
    esac
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source $(pwd)/das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh

simulation_list $@
exit $?
