function run_set1D_bunch_line_plot_2comp(cfg,stat)
    Tkind  = {'Insta','Taver','Tanom'};
    Ephase = {'prior','postr'};

    for j=1:length(Tkind)
        for k=1:length(Ephase)
            scalar.assi =['assi_' stat '_' Ephase{k} '_' Tkind{j}];
            scalar.free =['free_' stat '_' 'prior'   '_' Tkind{j}];
            create_plot(cfg,scalar.free,scalar.assi);
        end
    end
end

function create_plot(cfg,free_scalar,assi_scalar)
%==================================================================
% 2-panel line plot for 2 component models
%==================================================================

% Import data

file_path = ['../' cfg.rel_run_free_set_dir{:} '/stats/' free_scalar '.nc'];
scalar_free = get_scalar_netcdf(file_path);
for k=1:length(cfg.run_set_names)
    file_path = ['../run_sets/' cfg.run_set_names{k} '/stats/' assi_scalar '.nc'];
    scalar_assi(k) = get_scalar_netcdf(file_path);
end

% Create line plots

figure_hdl=figure();
set(figure_hdl,'Name',assi_scalar,'NumberTitle','off');
set(figure_hdl,'Position', [0 0 600 300]); clf;

marker_shapes = {'s','^','o','v'};
line_colors   = {'b','r','g'};
plot_width    = 0.3;   plot_height = 0.74 ;
plot_ypos     = 0.14;   plot_xpos   = [0.1  0.46];

for l=1:2
    axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
    hold on;

    plot(scalar_free.dim.data,scalar_free.var(l).data,...
        'LineStyle','-','Color','k','Marker','d',...
        'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor','k');

    for k=1:length(cfg.run_set_names)
        line_color   =   line_colors{cfg.bunch_par1_value_pos(k)+1};
        marker_shape = marker_shapes{cfg.bunch_par2_value_pos(k)+1};

        plot(scalar_assi(k).dim.data,scalar_assi(k).var(l).data,...
            'LineStyle','-','Color',line_color,'Marker',marker_shape,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
    end

    xlabel(scalar_free.dim.label);  
    if(l==1); ylabel(assi_scalar); end
    axis tight; 
    ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);

    title(scalar_free.var(l).label);
end

% Add Legend

format1   = {'HorizontalAlignment','left','FontSize',16};
format2   = {'HorizontalAlignment','left','FontSize',14};
line_opts = {'Color','k','LineWidth',2,'LineStyle','-','MarkerSize',10};

% Assi. ens. run
axes('pos',[0.77 0.25 0.23 0.6]); axis([0 1 0 0.8]);
set(gca, 'YTickLabel', [],'YTick',[]);
set(gca, 'XTickLabel', [],'XTick',[]);

ypos = 0.7; text(0.05,ypos,cfg.bunch_par1_label,format1{:});
for j1 =1:cfg.bunch_par1_span_length
    ypos = 0.7 - j1*0.1;
    text(0.05 ,ypos,cfg.bunch_par1_values{j1},format2{:},'Color',line_colors{j1});
end

ypos = 0.35;  text(0.05,ypos,cfg.bunch_par2_label,format1{:});
for j2 =1:cfg.bunch_par2_span_length
    ypos = 0.35 - j2*0.1;
    text(0.2 ,ypos,cfg.bunch_par2_values{j2},format2{:});
    line([0.1 0.1],[ypos ypos],'Marker',marker_shapes{j2},line_opts{:});
end

% free ens. run
axes('pos',[0.77 0.1 0.23 0.1]); axis([0 1 0 0.8]);
set(gca, 'YTickLabel', [],'YTick',[]);
set(gca, 'XTickLabel', [],'XTick',[]);

ypos = 0.35;
text(0.2 ,ypos,'Free ensemble',format2{:});
line([0.1 0.1],[ypos ypos],'Marker','d',line_opts{:});

% save

saveas(figure_hdl,[assi_scalar '.fig'],'fig');
export_fig('-transparent',[assi_scalar '.pdf'],figure_hdl);
%    export_fig('-transparent','-nocrop',[scalar '.eps'],figure_hdl);

close(figure_hdl);
end

