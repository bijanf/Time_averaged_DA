function run_set_plot(cfg)

    if  (isequal(cfg.run_mode{1},'free'))
        disp(' Only assi run sets supported');
        return
    end
    
    disp('  Importing netcdf files');
    cfg.dataset_dim=0;
    
    nature.dir   = ['../' cfg.rel_reference_dir{1}];
    nature.value = import_netcdf(cfg,[nature.dir '/raw_data/nature_Insta_all.nc']);
    nature_cfg   = get_cfg([nature.dir '/config']);
    cfg.cycle_length = nature_cfg.cycle_length;
    
    for j=1:length(cfg.run_names)
        assirun{j}.name  = cfg.par1_values{j};
        assirun{j}.label = label(cfg.par1_values{j},cfg.par1_name{:});
        
        assirun{j}.dir  = ['../runs/' cfg.run_names{j} '/raw_data'];
        assirun{j}.obs  = import_obs (cfg, [assirun{j}.dir '/obs_clean_Insta.nc']);
        assirun{j}.threshold_up   = import_netcdf (cfg, [assirun{j}.dir '/threshold_up.nc']);
        assirun{j}.threshold_down = import_netcdf (cfg, [assirun{j}.dir '/threshold_down.nc']);
    end
    
    cfg.nature = nature;
    cfg.assirun = assirun;
    
    model_run_set_plots(cfg)
end