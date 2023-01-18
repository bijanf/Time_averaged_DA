function run_set_plot(cfg)
    
    scalar= 'nature_grid_sigma_Taver_Zonmean_Tstdd';
    for irun = 1:length(cfg.run_names)
    file_path=['../runs/' cfg.run_names{irun} '/stats/' scalar '.nc'];
    nature_Zonmean = import_state(cfg,file_path);

    for ivar=1:4
        zonal_plot(nature_Zonmean,ivar,cfg.run_names{irun});
    end
    end
%figure
%levels = 0.0:0.001:0.997;
%contourf(nature_Zonmean.dim(2).data', nature_Zonmean.dim(3).data', ...
%     double(squeeze(nature_Zonmean.var(1).data(1,:,:)))','LineColor','none');

%    nature_Zonmean.var(2)
%    strucdisp(nature_Zonmean);
%     if(isequal(cfg.run_mode{1},'free'))
%         nature_dir='../raw_data';
%     else
%         nature_dir=['../' cfg.rel_run_free_dir{1} '/raw_data'];
%     end
%     
%     file_path=[nature_dir '/nature_Insta_all.nc'];
% %     file_path=[nature_dir '/nature_Insta.nc'];
%     nature_Insta.value = import_netcdf(cfg,file_path);
% 
%     file_path=[nature_dir '/nature_Insta_all_max.nc'];
%     nature_Insta.max = import_netcdf(cfg,file_path);
% 
%     file_path=[nature_dir '/nature_Insta_all_min.nc'];
%     nature_Insta.min = import_netcdf(cfg,file_path);
% 
%     file_path=[nature_dir '/nature_Insta_all_mean.nc'];
%     nature_Insta.mean = import_netcdf(cfg,file_path);
% 
%     file_path=[nature_dir '/nature_Insta_all_stdd.nc'];
%     nature_Insta.stdd = import_netcdf(cfg,file_path);
% 
%     nature_Insta.obs = import_obs(cfg,'../raw_data/obs_clean_Insta.nc');
% 
%     fprintf(2,' Nature plots \n');
%     model_nature_plots(cfg,nature_Insta.value);
% 
%     fprintf(2,' Observation plots \n');
%     if(isequal(cfg.run_mode{1},'assi'))
% %       observation_plots(cfg,nature_Insta);
%       model_obs_plots(cfg,nature_Insta);
%     end
    
%     % save
%     
%     for j=1:length(h)
%         plot_name = ['nature_plot_' num2str(j)];
%         saveas(h(j),[plot_name '.fig'],'fig');
%         export_fig('-transparent',[plot_name '.pdf'],h(j));
%     end
%     close all
    
end

function zonal_plot(state,ivar,run_name)
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 700 300]); clf;
    
    vardata = double(squeeze(state.var(ivar).data(1,:,:)))';

%    latdata = state.dim(2).data';
%    levdata = state.dim(3).data';
   
%    pcolor(state.dim(2).data', state.dim(3).data', vardata);
    imagesc(state.dim(2).data', state.dim(3).data', vardata);

    shading flat; 
    %    shading interp;
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');

    %color_axis = [0 10];
    xlim([-90 90]); ylim([  0  1]);
    
    title(state.var(ivar).label);
    xlabel(state.dim(2).label);
    if isequal(state.dim(3).label,'generic')
        state.dim(3).label = 'sigma';
    end
    ylabel(state.dim(3).label);
    
    set(gca,'XTick',[-90,-45,0,45,90])
    %set(gca,'YTick',[-1,-0.5,0,0.5,1])

    % Color scale
    hcb = colorbar;
    %ylabel(hcb, pretitle);
    
    plot_name = [ run_name '_Zonmean_' state.var(ivar).name];

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],figure_hdl);

    close(figure_hdl);

end

