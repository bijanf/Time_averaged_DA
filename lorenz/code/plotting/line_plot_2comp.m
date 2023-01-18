function line_plot_2comp(cfg,scalar)
%==================================================================
% 2-panel line plot for 2 component models
%==================================================================
    figure_hdl=figure();
    set(figure_hdl,'Name',scalar,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;
    set_par_name = 'Taver_length';

    marker_shapes = {'s','^','o'};
    line_colors   = {'b','r','g'};

    var_name     = {'comp1','comp2'};
    plot_title   = {'Fast Component','Slow Component'};

    plot_width   = 0.32;   plot_height = 0.74 ;
    plot_ypos    = 0.14;   plot_xpos   = [0.1  0.43];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;
        for k=1:length(cfg.run_set_names)

            file_path = ['../run_sets/' cfg.run_set_names{k} '/stats/' scalar '.nc'];

            ncid = netcdf.open(file_path,'NOWRITE');
            comp_id    = netcdf.inqVarID(ncid,var_name{l});
            comp_data  = netcdf.getVar(ncid,comp_id);
            
            set_par_id    = netcdf.inqVarID(ncid,set_par_name);
            set_par_data  = netcdf.getVar(ncid,set_par_id);
            set_par_label = netcdf.getAtt(ncid,set_par_id,'long_name');
            netcdf.close(ncid);

            line_color   =   line_colors{cfg.bunch_par1_value_pos(k)+1};
            marker_shape = marker_shapes{cfg.bunch_par2_value_pos(k)+1};
            plot(set_par_data,comp_data,'LineStyle','-','Color',line_color,...
                'Marker',marker_shape,'MarkerSize',8,'Linewidth',1, ...
                'MarkerEdgeColor',line_color);
%             plot(set_par_data,comp_data,marker_shape...
%                  ,'Color',line_color,'MarkerSize',8,'Linewidth',1 ...
%                  ,'MarkerEdgeColor',line_color);

            xlabel(set_par_label);  
            if(l==1); ylabel(scalar); end
%             if(l==1); ylabel('RMSE Reduction (%)'); end
            xlim([0.06 0.51]);
                              
            title(plot_title(l));
            if(l==2); set(gca, 'YTickLabel', []); end;
        end
    end


    % Legend
    axes('pos',[0.77 0.25 0.23 0.6]); axis([0 1 0 0.8]);
    set(gca, 'YTickLabel', [],'YTick',[]);
    set(gca, 'XTickLabel', [],'XTick',[]);

    format1   = {'HorizontalAlignment','left','FontSize',16};
    format2   = {'HorizontalAlignment','left','FontSize',14};
    line_opts = {'Color','k','LineWidth',2,'LineStyle','-','MarkerSize',10};

   
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

    export_fig('-transparent',[scalar '.pdf'],figure_hdl);
    %export_fig('-transparent','-nocrop',[scalar '.eps'],figure_hdl);
    saveas(figure_hdl,[scalar '.fig'],'fig');
    close(figure_hdl);
end