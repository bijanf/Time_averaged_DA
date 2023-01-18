function scalar = import_obs(cfg,file_path)
    var_name  = {'obs','comp2'};
    var_label = {'Fast Component','Slow Component'};

    for l=1:1
        ncid = netcdf.open(file_path,'NOWRITE');
        comp_id    = netcdf.inqVarID(ncid,var_name{l});
        comp_data  = netcdf.getVar(ncid,comp_id);
        fillValue  = netcdf.getAtt(ncid,comp_id,'_FillValue');
%         comp_label = netcdf.getAtt(ncid,comp_id,'long_name');
        comp_data(comp_data == fillValue) = nan;

        scalar.var(l).data  = comp_data;
        scalar.var(l).name  = var_name{l};
        scalar.var(l).label = var_label{l};
    end
    
    if(cfg.dataset_dim ==0)
        scalar.dim(1).name = 'time';
        scalar_dim_id      = netcdf.inqVarID(ncid,scalar.dim(1).name);
        scalar.dim(1).data = netcdf.getVar(ncid,scalar_dim_id);
        scalar.dim(1).data = cfg.cycle_length * scalar.dim(1).data;
    else    
        
        set_par_name = 'Taver_length';
        set_par_id    = netcdf.inqVarID(ncid,set_par_name);
        set_par_data  = netcdf.getVar(ncid,set_par_id);
        set_par_label = netcdf.getAtt(ncid,set_par_id,'long_name');
        netcdf.close(ncid);
        
        scalar.dim.data   = set_par_data;
        scalar.dim.label  = set_par_label;
    end
end