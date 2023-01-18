function scalar = get_scalar_netcdf(cfg,file_path)
    var_name  = {'comp1','comp2'};
    var_label = {'Fast Component','Slow Component'};

    ncid = netcdf.open(file_path,'NOWRITE');

    for l=1:2
        comp_id    = netcdf.inqVarID(ncid,var_name{l});
        comp_data  = netcdf.getVar(ncid,comp_id);
        fillValue  = netcdf.getAtt(ncid,comp_id,'_FillValue');
%         comp_label = netcdf.getAtt(ncid,comp_id,'long_name');
        comp_data(comp_data == fillValue) = nan;

        scalar.var(l).data  = comp_data;
        scalar.var(l).name  = var_name{l};
        scalar.var(l).label = var_label{l};
    end
    
    switch cfg.set_dim
         case 1
             par_name{1} = cfg.set_par1_name{:};
         case 2
             par_name{1} = cfg.set_par1_name{:};
             par_name{2} = cfg.set_par2_name{:};
    end

    for j=1:cfg.set_dim
        scalar.dim(j).name = par_name{j};
    %     set_par_name = 'Taver_length';
        set_par_id    = netcdf.inqVarID(ncid,scalar.dim(j).name);
        scalar.dim(j).data  = netcdf.getVar(ncid,set_par_id);
        scalar.dim(j).label = label(par_name{j},'par');
%         scalar.dim(j).label = netcdf.getAtt(ncid,set_par_id,'long_name');
    end
            
    netcdf.close(ncid);

end