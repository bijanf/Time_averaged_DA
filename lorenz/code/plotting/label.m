function string_label = label(string,criterion)
    
    dict.Ephase.prior      = 'Forecast';
    dict.Ephase.postr      = 'Analysis';
    dict.Ephase.nature     = 'Nature';
    dict.Ephase.free       = 'Free Run';
    
    
    dict.Tkind.Insta       = 'Instantaneous';
    dict.Tkind.Taver       = 'Time-Averaged';
%     dict.Tkind.Tanom       = 'Time Perturbations';
    dict.Tkind.Tanom       = 'Time Perturb.';
    
    
    dict.par.cycle_length  = 'Observation period';
    dict.par.cycles        = 'Assimilation cycles';
    dict.par.Taver_length  = 'Time average length';
    dict.par.infl_enkf     = 'Inflation factor';
    dict.par.infl_mode     = 'Inflation mode';
    dict.par.SNR           = 'SNR';
    dict.par.update_mode   = 'Updated State';
    dict.par.loc_radius    = 'Localization radius';
%     dict.par.obs_operator  = 'Obs. Operator';
    dict.par.obs_operator  = 'Combination';
    dict.par.Tkind         = '';
    dict.par.comp_localization = 'Comp. Localization';

    
%     dict.par.range_lower = '$X_1$';
%     dict.par.range_upper = '$X_2$';
    dict.par.range_lower = 'Lower Limit';
    dict.par.range_upper = 'Upper Limit';

    
    dict.stat.error_Tstdd_reduc = 'Error Reduction (%)';
    dict.stat.Esprd_Tmean_reduc = 'Spread Reduction (%)';
    dict.stat.Emean_Tstdd_reduc = 'Std. Dev. Reduction (%)';
    dict.stat.Esprd_Tmean       = 'Spread'; 
    dict.stat.error_Tstdd       = 'RMSE';
    dict.stat.Emean_Tstdd       = 'Standard Deviation';
    dict.stat.nature_Tstdd      = 'Nature Standard Deviation';

    
    dict.update_mode.Insta = 'Instantaneous';
    dict.update_mode.Hakim = 'Time Averaged';
    dict.update_mode.Augm0 = 'Time Augmented';
    dict.update_mode.Augm1 = 'Time Augmented';
    dict.update_mode.Augm2 = 'Taver + All Tanom.';
%     dict.update_mode.Augm3 = 'Taver + Last Tanom.';
    dict.update_mode.Augm3 = 'Taver + Last Insta.';
    dict.update_mode.Augm4 = 'Taver + Last Insta.';
    
    
    dict.infl_mode.step_   = 'Infl. every step';
    dict.infl_mode.cycle   = 'Infl. every cycle';
    
    
    dict.obs_operator.Resp_comp1   = '$H^{Fast}$';
    dict.obs_operator.Resp_comp2   = '$H^{Slow}$';
    dict.obs_operator.Resp_add         = 'Linear';  %'Addition';
    dict.obs_operator.Resp_min         = 'Minimum';      %'min(Comp. 1,2)'; %
    dict.obs_operator.Resp_product     = 'Product';
    dict.obs_operator.Resp_lukasiewicz = 'Lukasiewicz';
    dict.obs_operator.Resp_yager       = 'Yager';
    

    % old operators
    dict.obs_operator.addi = 'addi';
    dict.obs_operator.plus = 'Addition';
    dict.obs_operator.vsl0 = 'Minimum';
    dict.obs_operator.vsl1 = 'Product';
%     dict.obs_operator.plus = 'Linear';
%     dict.obs_operator.vsl0 = 'VS-Lite-like';
%     dict.obs_operator.vsl1 = 'VS-Lite smooth';

%     dict.obs_operator.Resp_comp1 = 'Comp. 1';
%     dict.obs_operator.Resp_comp2 = 'Comp. 2';
%     dict.obs_operator.Resp_add   = 'Comp. 1 + 2';  %'Addition';
%     dict.obs_operator.Resp_min   = 'VS-lite';      %'min(Comp. 1,2)'; %
%     dict.obs_operator.Resp_product = 'VSL-smooth';

    
    
    

 
    dict.comp_localization.yes = 'yes';
    dict.comp_localization.no  = 'no';
%     dict.coupled_analysis.yes = 'Coupled';
%     dict.coupled_analysis.no  = 'Uncoupled';
 
    %    dict.tao.Emean_Tstdd_reduc = 'Std. Dev. Reduction (%)';

    if isequal(criterion,'SNR')
        string_label = string;
    else
        label_dict = dict.(criterion);
        sub_string = fieldnames(label_dict);
        for j=1:length(sub_string)
            if (~isempty(strfind(string,sub_string{j})))
                string_label = label_dict.(sub_string{j});
                break
            end
        end
    end
end


    %     dict.run.free = 'Free Forecast';
%                 *assi*prior*) echo "D.A. Forecast";;
%                 *assi*postr*) echo "D.A. Analysis";;
%                 *assi*      ) echo "D.A. Ensemble";;
%                 *nature*    ) echo "Nature Run";;
%                 *           ) echo "\url{$string}"; error_message;;
%             esac;;
%         Quantity)
%             case "$string" in
%                 *Emean_Tstdd*reduc*) echo "State Std. Deviation Reduction";;
%                 *Emean_Tstdd*      ) echo "State Std. Deviation";;
%                 *error*reduc*      ) echo "RMSE Reduction";;
%                 *obs_error_stdd*   ) echo "Obs. Error Std. Deviation";;
%                 *error*            ) echo "RMSE";;
%                 *Esprd*reduc*      ) echo "Spread Reduction";;
%                 *Esprd*            ) echo "Spread";;
%                 *nature_Tstdd*     ) echo "State Std. Deviation";;


