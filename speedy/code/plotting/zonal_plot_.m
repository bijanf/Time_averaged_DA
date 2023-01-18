function zonal_plot_(var,plot_name)
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 700 300]); clf;
    
%    vardata = double(squeeze(state.var(ivar).data(1,:,:)))';

%     latdata = state.dim(2).data';
%     levdata = state.dim(3).data';
%    [lat,lev] = meshgrid(latdata,levdata);
%    size(lat)
%    size(lev)
%    size(vardata)
%    imagesc(lat,lev, vardata);
   
%    pcolor(var.lat.data, var.lev.data, squeeze(var.data)');
    imagesc(var.lat.data, var.lev.data, squeeze(var.data)');

    shading flat; 
    %    shading interp;
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');

    %color_axis = [0 10];
    xlim([-90 90]); ylim([  0  1]);
    
    title(var.label); 
    xlabel(var.lat.label); ylabel(var.lev.label);
    
    set(gca,'XTick',[-90,-45,0,45,90])
    %set(gca,'YTick',[-1,-0.5,0,0.5,1])

    % Color scale
    hcb = colorbar;
    %ylabel(hcb, pretitle);
    
%    plot_name = [ run_name '_Zonmean_' state.var(ivar).name];

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],figure_hdl);

    close(figure_hdl);

end