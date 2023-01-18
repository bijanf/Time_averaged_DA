function scalar = import_state(cfg,file_path)
    ncid = netcdf.open(file_path,'NOWRITE');
    var_name  = {'u','v','t','q'};
    dim_name  = {'lon','lat','lev'};

    for l=1:length(var_name)
        var_id    = netcdf.inqVarID(ncid,var_name{l});
        var_data  = netcdf.getVar(ncid,var_id);
        fillValue  = netcdf.getAtt(ncid,var_id,'_FillValue');
        var_label = netcdf.getAtt(ncid,var_id,'long_name');
        var_data(var_data == fillValue) = nan;

        scalar.var(l).data  = var_data;
        scalar.var(l).name  = var_name{l};
        scalar.var(l).label = var_label;
    end
    
    for l=1:length(dim_name)
        scalar.dim(l).name  = dim_name{l};
        scalar_dim_id       = netcdf.inqVarID(ncid,scalar.dim(l).name);
        scalar.dim(l).data  = netcdf.getVar(ncid,scalar_dim_id);
        scalar.dim(l).label = netcdf.getAtt(ncid,scalar_dim_id,'long_name');
    end
    netcdf.close(ncid);
%     if(cfg.dataset_dim ==0)
%         scalar.dim(1).name = 'time';
%         scalar_dim_id      = netcdf.inqVarID(ncid,scalar.dim(1).name);
%         scalar.dim(1).data = netcdf.getVar(ncid,scalar_dim_id);
%         scalar.dim(1).data = cfg.cycle_length * scalar.dim(1).data;
%     else    
%         
%         set_par_name = 'Taver_length';
%         set_par_id    = netcdf.inqVarID(ncid,set_par_name);
%         set_par_data  = netcdf.getVar(ncid,set_par_id);
%         set_par_label = netcdf.getAtt(ncid,set_par_id,'long_name');
%         netcdf.close(ncid);
%         
%         scalar.dim.data   = set_par_data;
%         scalar.dim.label  = set_par_label;
%     end
end