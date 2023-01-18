function var = get_var_from_netcdf(cfg,file_path,var_name)
    ncid = netcdf.open(file_path,'NOWRITE');

    var.id    = netcdf.inqVarID(ncid,var_name);
    [var.name, xtype, dim_ids, atts] = netcdf.inqVar(ncid,var.id);

    var.data  = netcdf.getVar(ncid,var.id);
    fillValue = netcdf.getAtt(ncid,var.id,'_FillValue');

    if isequal(var_name,'q')
        var.data(var.data == fillValue) = 0.0;
%         var.data(var.data == fillValue) = nan;
    else
        var.data(var.data == fillValue) = nan;
    end
    
    var.data  = double(var.data);

    var.label = netcdf.getAtt(ncid,var.id,'long_name');
    var.data(var.data == fillValue) = nan;

    for l=1:length(dim_ids)
        [dim_name, xtype, dim_id, atts] = netcdf.inqVar(ncid,dim_ids(l));

        var.(dim_name).name  = dim_name;
        var.(dim_name).id    = dim_id;
        var.(dim_name).data  = netcdf.getVar(ncid, dim_id);
        
        if isequal(dim_name,'time')
            var.(dim_name).label = 'time';
        else
            var.(dim_name).label = netcdf.getAtt(ncid, dim_id,'long_name');
        end
        
        
        if isequal(dim_name,'lev')
            if isequal(var.lev.label,'generic')
                var.lev.label = 'sigma';
            end
        end
    end

    netcdf.close(ncid);
end