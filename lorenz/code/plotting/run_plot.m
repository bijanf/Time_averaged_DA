function run_plot(cfg)
    print_function_message('opening')

    if  (isequal(cfg.run_mode{1},'free'))
        nature_dir='../raw_data';
    else
        nature_dir=['../' cfg.rel_run_free_dir{1} '/raw_data'];
    end
    
    file_path=[nature_dir '/nature_Insta_all.nc'];
%     file_path=[nature_dir '/nature_Insta.nc'];
    nature_Insta.value = import_netcdf(cfg,file_path);

    file_path=[nature_dir '/nature_Insta_all_max.nc'];
    nature_Insta.max = import_netcdf(cfg,file_path);

    file_path=[nature_dir '/nature_Insta_all_min.nc'];
    nature_Insta.min = import_netcdf(cfg,file_path);

    file_path=[nature_dir '/nature_Insta_all_mean.nc'];
    nature_Insta.mean = import_netcdf(cfg,file_path);

    file_path=[nature_dir '/nature_Insta_all_stdd.nc'];
    nature_Insta.stdd = import_netcdf(cfg,file_path);

    if exist('../raw_data/threshold_down.nc','file')
        nature_Insta.threshold_down = ...
            import_netcdf(cfg,'../raw_data/threshold_down.nc');
    end
    if exist('../raw_data/threshold_up.nc','file')
        nature_Insta.threshold_up = ...
            import_netcdf(cfg,'../raw_data/threshold_up.nc');
    end
%     nature_Insta.threshold_up = ...
%         import_netcdf(cfg,'../raw_data/threshold_up.nc');


    fprintf(2,' Nature plots \n');
    model_nature_plots(cfg,nature_Insta.value);

    fprintf(2,' Observation plots \n');
    if(isequal(cfg.run_mode{1},'assi'))
        nature_Insta.obs = import_obs(cfg,'../raw_data/obs_clean_Insta.nc');
        %       observation_plots(cfg,nature_Insta);
        model_obs_plots(cfg,nature_Insta);
    end
    
%     % save
%     
%     for j=1:length(h)
%         plot_name = ['nature_plot_' num2str(j)];
%         saveas(h(j),[plot_name '.fig'],'fig');
%         export_fig('-transparent',[plot_name '.pdf'],h(j));
%     end
%     close all
    print_function_message('closing')
end