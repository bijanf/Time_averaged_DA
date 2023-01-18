function h = plot_stat_vs_par1_par2(dae, stat, my_colormap, color_axis, pretitle, varargin)
    
    par1_label = dae.par1_name; par2_label = dae.par2_name;
    
    plot_title = [pretitle ' vs ' par1_label '_' par2_label '. '];
    if nargin > 4
        plot_title = [plot_title varargin{1}];
    end
    h = figure('name',plot_title,'numbertitle','off');
    
    pcolor(dae.par1_span, dae.par2_span, stat'); shading flat
    %levels = 0.0:0.001:0.997; %contourf(dae.par1_span, dae.par2_span, stat',levels,'LineColor','none');
    
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');
    colormap(my_colormap); caxis(color_axis);
    hline = refline(1,-.01); set(hline,'Color','k','linewidth',1);
    
    ylabel(dae.par1_name); xlabel(dae.par2_name);
    title(run_label(dae,' '),'FontSize',10);
    
    xlim([dae.par1_span(1) dae.par1_span(end)]);
    ylim([dae.par2_span(1) dae.par2_span(end)]);
    
    % Color scale
    hcb = colorbar;
    ylabel(hcb, pretitle);
end
