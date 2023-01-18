function zonal_plot(state,ivar,run_name)

    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 700 300]); clf;
    
    vardata = double(squeeze(state.var(ivar).data(1,:,:)))';

    imagesc(state.dim(2).data', state.dim(3).data', vardata);
    % pcolor(state.dim(2).data', state.dim(3).data', vardata);
    % !! using pcolor not all the lines are plotted !!
    % see http://meyavuz.blogspot.de/2011/12/difference-between-pcolor-and-imagesc.html

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
