clear all; close all
CODE_DIR = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting']);

startup

[status, dataset_dir] = system('cd ..; pwd');
cfg = get_cfg([strtrim(dataset_dir) '/config']);

%==================================================================
% Error reduction Plot for 2 component models
%==================================================================

for j=1:length(cfg.scalars)
    prefix = strtrim(cfg.scalars{j})
    figure_hdl=figure();
    set(figure_hdl,'Name',prefix,'NumberTitle','off');
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

            file_path = ['../run_sets/' cfg.run_set_names{k} '/stats/' prefix '.nc'];

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
            if(l==1); ylabel(prefix); end
%             if(l==1); ylabel('RMSE Reduction (%)'); end
            xlim([0.06 0.51]);
                
            if (~isempty(strfind(prefix,'Emean_Tstdd_reduc')))
                ylim([-1000 0])
            elseif (~isempty(strfind(prefix,'reduc')))
                ylim([0 100])
            else
                ylim([0 10])
            end
              
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

    export_fig('-transparent',[prefix '.pdf'],figure_hdl);
    %export_fig('-transparent','-nocrop',[prefix '.eps'],figure_hdl);
    saveas(figure_hdl,[prefix '.fig'],'fig');
    close(figure_hdl);
end
              
%     ypos = 0.7; text(0.05,ypos,'D.A. Approach' ,format1{:});
%     ypos = 0.6; text(0.05,ypos,'Time Averaged' ,format2{:},'Color','b');
%     ypos = 0.5; text(0.05,ypos,'Time Augmented',format2{:},'Color','r');

%     ypos = 0.35; text(0.05,ypos,'Obs. Operator',format1{:});
%    ypos = 0.25; text(0.2,ypos,'Linear',format2{:});
%      line([0.1 0.1],[ypos ypos],'Marker','s',line_opts{:});
%     ypos = 0.15; text(0.2,ypos,'VS-Lite-like',format2{:});
%     line([0.1 0.1],[ypos ypos],'Marker','^',line_opts{:});
% 
%     ypos = 0.05; text(0.2,ypos,'VS-Lite smooth',format2{:});
%     line([0.1 0.1],[ypos ypos],'Marker','o',line_opts{:});
                
