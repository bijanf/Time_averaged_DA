#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source ./initialize_das.sh
source $CODE_DIR/running_scripts/common_tools.sh
source $CODE_DIR/running_scripts/experiments.sh

check_dependencies

declare -A span unit

#=========================================================
# @brief Running script for speedy model
#=========================================================
model_script(){
    sim_id=$1

    set_run_paths

    case $sim_id in
        test )
            source $CODE_DIR/running_scripts/test_list.sh
            ;;
#         test_list ;;
        # ---------------------------------------------
        #  Batch 5
        # ---------------------------------------------
        exp01)
            SST="clim_81_90"; surf_mode="prescribed"
            init_runs
            line "cycle_length"
            #single_run
            ;;
        exp02)
            init_runs
            line "cycle_length"
            #single_run
            ;;

        exp03)
            SST="79_90"; surf_mode="slab_model"; verity=2;
            station_set_number=2;
            init_runs; line "cycle_length";
            ;;
        exp04)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=7; source "$MODEL_DIR/model_config.sh"

            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp05)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=7; export obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"

            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp06)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=7; export obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"

            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp07)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=7; export obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"

            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp08)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=7; export obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"

            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp09)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=8; export obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"
            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error ;;
        exp10)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=8; export obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp11)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=8; export obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"
            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp12)
            SST="79_90"; surf_mode="slab_model"; export verity=2;
            station_set_number=8; export obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"
            run_set.sh --run_type=assi --ref_set_dir="$(free_run_set_dir)" \
                "$(assi_run_set_dir)" || error
            ;;
        exp13)
            SST="50_99"; surf_mode="slab_model"; export verity=3;
            # station_set_number=8; #export obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            set_par1_name=cycle_length
            run.sh --run_type=spinup \
                "$(spinup_dir)"       || error
            run.sh --run_type=sampling   --spinup_dir="$(spinup_dir)" \
                "$(sampling_dir)"     || error
            run_set.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                --par1_name=$set_par1_name \
                --par1_span=${span[$set_par1_name]} \
                --par1_unit=${unit[$set_par1_name]} \
                "$(natu_run_set_dir)" || error
            ;;
        # ---------------------------------------------
        #  Batch 6
        # ---------------------------------------------
        exp20)
            surf_mode="slab_model"; export verity=3;
            source "$MODEL_DIR/model_config.sh"

            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
        exp21)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                "$(assi_run_norm_addObs_dir)" || error
            ;;
        exp22)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        exp23)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        exp24)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        # ---------------------------------------------
        #  Batch 7 (Skewness calculation added)
        # ---------------------------------------------
        exp30)
            surf_mode="slab_model"; export verity=3;
            source "$MODEL_DIR/model_config.sh"

            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
        exp31)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                "$(assi_run_norm_addObs_dir)" || error
            ;;
        exp32)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        exp33)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        exp34)
            surf_mode="slab_model"; export verity=3;
            station_set_number=8; obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normAdd_obs_dir="$(assi_run_normAdd_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;
	
        exp40)
            surf_mode="slab_model"; export verity=3;
            source "$MODEL_DIR/model_config.sh"

            # run.sh --run_type=spinup     "$(spinup_dir)" || error
            # run.sh --run_type=sampling \
            #     --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
            
        # ---------------------------------------------
        #  Batch 10 (Skewness calculation added)
        # ---------------------------------------------
        exp50)
            surf_mode="slab_model"; export verity=5;
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
        exp51)
            surf_mode="prescribed"; export verity=5;
            station_set_number=8; obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                "$(assi_run_norm_addObs_dir)" || error
            ;;
                    
        exp52)
            surf_mode="slab_model"; export verity=5;
            station_set_number=8; obs_operator='norm_T_half'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normThalf_obs_dir="$(assi_run_normThalf_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;      
        exp53)
            surf_mode="prescribed"; export verity=5;
            station_set_number=8; obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normT_obs_dir="$(assi_run_normT_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;    
            
        exp54)
            surf_mode="prescribed"; export verity=5;
            station_set_number=8; obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normMin_obs_dir="$(assi_run_normMin_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;
            
         exp55)
            surf_mode="prescribed"; export verity=5;
            station_set_number=8; obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normProd_obs_dir="$(assi_run_normProd_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;   
                            
        exp56)
            surf_mode="prescribed";export verity=5;
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
               "$(natu_run_dir)" || error
            ;;   
           # run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
           #     --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
           # ;;        
           
        exp57)
            surf_mode="prescribed";export verity=5;
            source "$MODEL_DIR/model_config.sh"
            #run.sh --run_type=spinup     "$(spinup_dir)" || error
            #run.sh --run_type=sampling \
            #    --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
           # run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #    "$(natu_run_dir)" || error
           # ;;   
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
           
           
           
           
           
           
                               
        exp41) # modified by Bijan 
			SST="1854_2010";surf_mode="slab_model"; export verity=2;
            station_set_number=8; obs_operator='norm_min'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_norm_addObs_dir="$(assi_run_norm_addObs_dir)" \
                "$(assi_run_dir)" || error
            ;;
        exp42) # modified by Bijan
            SST="1854_2010"; surf_mode="slab_model"; export verity=5;
            init_runs
            line "cycle_length"
            #single_run
            ;;
               
        exp43)
            surf_mode="slab_model"; export verity=5;
            station_set_number=8; obs_operator='norm_T'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                  "$(assi_run_dir)" || error
            ;;
        exp44)
            surf_mode="slab_model"; export verity=5;
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;
        exp45)
            surf_mode="slab_model"; export verity=5;
            station_set_number=8; obs_operator='norm_add'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                "$(assi_run_norm_addObs_dir)" || error
            ;;
        # ---------------------------------------------
        #  Batch 11 (slab only)
        # ---------------------------------------------    
        exp60)
            surf_mode="slab_model"; export verity=6;
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;;    
            
            
         exp61)
            surf_mode="slab_model"; export verity=6;
            station_set_number=8; obs_operator='norm_prod'
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
                --assi_run_normProd_obs_dir="$(assi_run_normProd_obs_dir)" \
                "$(assi_run_dir)" || error
            ;;    
        exp62)
            surf_mode="slab_model"; export verity=6;
            source "$MODEL_DIR/model_config.sh"
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=sampling \
                --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
                --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
            ;; 
        # ---------------------------------------------
        #  Batch 12 (slab + prescribed, prescribed and slab-only )
        # ---------------------------------------------    

        exp100)
        
            # dependeny on initialization method
            
            export verity=6; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="slab_plus_pres"; 
            station_set_number=8; obs_operator='norm_T'
            
            run.sh --run_type=spinup     "$(spinup_dir)" || error
            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error
            
			sampling_free_assi(){
				sampling_step=$1; sampling_step_unit="dy" ;
				run.sh --run_type=sampling \
					--spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error
				run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
					--nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error
				run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
					"$(assi_run_dir)" || error
			}

			sampling_free_assi 2
			sampling_free_assi 4
			sampling_free_assi 8
			sampling_free_assi 16
			sampling_free_assi 32
            ;;    
	
	exp101) #norm_T slab_plus_pres
	    export verity=7; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="slab_plus_pres"; 
            station_set_number=8; obs_operator='norm_T'
            
           run.sh --run_type=spinup  "$(spinup_dir)" || error
            
            run.sh --run_type=sampling \
              --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

            run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error

           run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		    --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
            "$(asso_run_dir)" || error
	    
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
            "$(assi_run_dir)" || error
        
        ;; 
      exp102) #norm_T prescribed
	        export verity=6; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="prescribed"; 
            station_set_number=8; obs_operator='norm_T'
            
           run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           run.sh --run_type=sampling \
              --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

           run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
                "$(natu_run_dir)" || error

           run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		    --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
            "$(asso_run_dir)" || error
	    
           run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
            "$(assi_run_dir)" || error
        
        ;;
      
      exp103) #norm_T slab-only
	        export verity=8; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="slab_model"; 
            station_set_number=8; obs_operator='norm_T'
            
          # run.sh --run_type=spinup  "$(spinup_dir)" || error
            
          # run.sh --run_type=sampling \
          #    --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

          # run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #     "$(natu_run_dir)" || error

          # run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		  #  --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

          # run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
          #  "$(asso_run_dir)" || error
	    
           run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
            "$(assi_run_dir)" || error
        
        ;;
       exp104) #norm_prod prescribed -> to be run on Soroban
	        export verity=5; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="prescribed"; 
            station_set_number=8; obs_operator='norm_prod'
            
           #run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           #run.sh --run_type=sampling \
           #   --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

           #run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #     "$(natu_run_dir)" || error

           #run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		   # --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           #run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
           # "$(asso_run_dir)" || error
	    
          run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
           "$(assi_run_dir)" || error
        
        ;; 
       exp105) #norm_min prescribed -> to be run on Soroban
	        export verity=5; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="prescribed"; 
            station_set_number=8; obs_operator='norm_min'
            
           #run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           #run.sh --run_type=sampling \
           #   --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

           #run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #     "$(natu_run_dir)" || error

           run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		    --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           #run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
           # "$(asso_run_dir)" || error
	    
           run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
            "$(assi_run_dir)" || error
        
        ;;  
        
        exp106) #norm_min prescribed -> to be run on Soroban
	        export verity=5; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            surf_mode="slab_model"; 
            station_set_number=8; obs_operator='norm_min'
            
           #run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           #run.sh --run_type=sampling \
           #   --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

           #run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #     "$(natu_run_dir)" || error

           #run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		   # --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
            "$(asso_run_dir)" || error
	    
           #run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
           # "$(assi_run_dir)" || error
        
        ;; 
        
      exp107) #norm_min prescribed -> to be run on Soroban
	        export verity=5; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            #surf_mode="prescribed"; 
            surf_mode="slab_model";
            station_set_number=8; obs_operator='norm_T'
            
           # run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           # run.sh --run_type=sampling \
            #    --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

            #run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
            #    "$(natu_run_dir)" || error

          #  run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		  #   --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           #run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
           # "$(asso_run_dir)" || error
	    
           run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
            "$(assi_run_dir)" || error
        
        ;;   
       exp108) #norm_min prescribed -> to be run on Soroban
	        export verity=5; # verity should be always set before sourcing model_config
            source "$MODEL_DIR/model_config.sh"
            #surf_mode="prescribed"; 
            surf_mode="slab_model";
            station_set_number=8; obs_operator='norm_prod'
            
           # run.sh --run_type=spinup  "$(spinup_dir)" || error
            
           # run.sh --run_type=sampling \
           #     --spinup_dir="$(spinup_dir)" "$(sampling_dir)" || error

           # run.sh --run_type=nature --spinup_dir="$(spinup_dir)" \
           #     "$(natu_run_dir)" || error

            run.sh --run_type=free --sampling_dir="$(sampling_dir)" \
		     --nature_dir="$(natu_run_dir)" "$(free_run_dir)" || error

           #run.sh --run_type=asso --free_run_dir="$(free_run_dir)" \
           # "$(asso_run_dir)" || error
	    
            run.sh --run_type=assi --free_run_dir="$(free_run_dir)" \
             "$(assi_run_dir)" || error
        
        ;;   
        # ---------------------------------------------
        #  Default
        # ---------------------------------------------
        *    ) echo "No experiment with id $exp_id" 1>&2   ;;
    esac
}
set_run_paths(){
    case "$batch_name" in
        "batch12" ) 
			exp_dir      (){ echo "$STORE/$model/SST-${SST}__${surf_mode}";}
			spinup_id    (){ echo "start${spinup_start}__${spinup_length}${spinup_length_unit}";}
			spinup_dir   (){ echo "$(exp_dir)/spinup_run__$(spinup_id)";}
			natu_run_dir (){ echo "$(exp_dir)/natu_run";}

			sampling_id  (){ echo "SamplStep${sampling_step}${sampling_step_unit}_m${sampling_size}";}
			sampling_dir (){ echo "$(exp_dir)/sampling_run__$(sampling_id)";}

			free_run_dir (){ echo "$(exp_dir)/free_run__$(sampling_id)";}
			assi_run_id  (){ echo "hLoc500km__stationSet${station_set_number}__obs_op_${obs_operator}";}
			asso_run_id  (){ echo "hLoc500km__stationSet${station_set_number}__obs_op_${obs_operator}";}
			assi_run_dir (){ echo "$(exp_dir)/assi_run__$(sampling_id)__$(assi_run_id)";}
			asso_run_dir (){ echo "$(exp_dir)/asso_run__$(sampling_id)__$(asso_run_id)";}
			;;
       "batch11" | "batch10" )
			spinup_dir  (){
				echo "$(exp_dir)/spinup_run__start${spinup_start}__${spinup_length}${spinup_length_unit}";}
			sampling_dir(){
				echo "$(exp_dir)/sampling_run__${sampling_step}${sampling_step_unit}_m${sampling_size}";}
			length_dir  (){
				echo "$(exp_dir)/run_length_${run_length}${run_length_unit}";}
			natu_run_dir (){
				echo "$(length_dir)/natu_run";}
			free_run_dir (){
				echo "$(length_dir)/free_run__m${ensemble_size}";}
			assi_run_norm_addObs_dir(){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_norm_add";}
			assi_run_normT_Obs_dir(){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_norm_T";}
			assi_run_normMin_Obs_dir(){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_norm_min";}
			assi_run_normThalf_Obs_dir(){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_norm_T_half";}
			assi_run_normProd_Obs_dir(){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_norm_prod";}
			assi_run_dir (){
				echo "$(length_dir)/assi_run__m${ensemble_size}__hLoc500km__InfFac1__stationSet${station_set_number}__obs_op_${obs_operator}";}
			;;
	    	* ) echo "Current Paths do not work with $batch_name"	;;
	esac



}



model_script $@
