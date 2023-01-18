clear; close all
CODE_DIR    = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting']);

startup

dataset_dir = getenv('dataset_dir');
cfg = get_cfg([dataset_dir '/config']);
cfg.bunch_par1_values
cfg.bunch_par2_values
cfg.run_set_names
%==================================================================
% Error reduction Plot for 2 component models
%==================================================================
prefixes = {'assi_error_Tstdd_reduc_Xsel_postr_Insta', ...
            'assi_error_Tstdd_reduc_Xsel_prior_Insta', ...
            'assi_error_Tstdd_reduc_Xsel_prior_Taver', ...
            'assi_error_Tstdd_reduc_Xsel_postr_Taver', ...
            'assi_error_Tstdd_reduc_Xsel_prior_Tanom', ...
            'assi_error_Tstdd_reduc_Xsel_postr_Tanom'};

for j=1:length(prefixes)
    prefix = prefixes{j};
    figure_hdl=figure();
    set(figure_hdl,'Name',prefix,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;
    %    line_labels = importdata([prefix '_labels.txt']);
    %    plot_title  = importdata([prefix  '_title.txt']);
    par_name='Taver_length';

    dir_path = {...
        'update_mode-Hakim_obs_operator-plus',...
        'update_mode-Hakim_obs_operator-vsl0',...
        'update_mode-Hakim_obs_operator-vsl1',...
        'update_mode-Augm1_obs_operator-plus',...
        'update_mode-Augm1_obs_operator-vsl0',...
        'update_mode-Augm1_obs_operator-vsl1'};
    
    marker_shapes= {'-s','-^','-o'};
    marker_shape= {'-s','-^','-o','-s','-^','-o'};
    %     marker_shape= {'--s','--^','--o','--s','--^','--o'};
    line_color   = {'b'  ,'b'  ,'b'  ,'r'  ,'r'  ,'r'};
    %     line_color  = {'k'  ,'k'  ,'k'  ,'k'  ,'k'  ,'k'};
    marker_color = {'b'  ,'b'  ,'b'  ,'r'  ,'r'  ,'r'};
    var_name     = {'comp1','comp2'};
    plot_width   = 0.32;   plot_height = 0.74 ;
    plot_ypos    = 0.14;   plot_xpos   = [0.1  0.43];
    plot_title   = {'Fast Component','Slow Component'};

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;
        for k=1:length(dir_path)
            file_path = ['../run_sets/' cfg.run_set_names{k} '/stats/' \
			 prefix '.nc'];
	    marker_shape = marker_shapes{cfg.bunch_par1_value_pos{k}+1}
%            file_path = ['../run_sets/' dir_path{k} '/stats/' prefix '.nc'];
            ncid = netcdf.open(file_path,'NOWRITE');
            comp_id   = netcdf.inqVarID(ncid,var_name{l});
            comp_data = netcdf.getVar(ncid,comp_id);
            Taver_length_id   = netcdf.inqVarID(ncid,par_name);
            Taver_length_data = netcdf.getVar(ncid,Taver_length_id);
            netcdf.close(ncid);

            plot(Taver_length_data,comp_data,marker_shape...
                 ,'Color',line_color{k},'MarkerSize',8,'Linewidth',1 ...
                 ,'MarkerEdgeColor',marker_color{k});
%            plot(Taver_length_data,comp_data,marker_shape{k}...
%                 ,'Color',line_color{k},'MarkerSize',8,'Linewidth',1 ...
%                 ,'MarkerEdgeColor',marker_color{k});
            xlabel('Time averaging length');      % ylabel(plot_title);
            if(l==1); ylabel('RMSE Reduction (%)'); end
            xlim([0.06 0.51]); ylim([0 100]);
            title(plot_title(l));
            if(l==2); set(gca, 'YTickLabel', []); end;
        end
    end




    % Legend
    axes('pos',[0.77 0.25 0.23 0.6]); axis([0 1 0 0.8]);
    set(gca, 'YTickLabel', [],'YTick',[]);
    set(gca, 'XTickLabel', [],'XTick',[]);

    format1 = {'HorizontalAlignment','left','FontSize',16};
    %     format1 = {'HorizontalAlignment','left','FontSize',14,'FontWeight','bold'};
    format2 = {'HorizontalAlignment','left','FontSize',14};
    line_opts = {'Color','k','LineWidth',2,'LineStyle','-','MarkerSize',10};

    ypos = 0.7; text(0.05,ypos,'D.A. Approach' ,format1{:});
    ypos = 0.6; text(0.05,ypos,'Time Averaged' ,format2{:},'Color','b');
    ypos = 0.5; text(0.05,ypos,'Time Augmented',format2{:},'Color','r');

    ypos = 0.35; text(0.05,ypos,'Obs. Operator',format1{:});
    ypos = 0.25; text(0.2,ypos,'Linear',format2{:});
    line([0.1 0.1],[ypos ypos],'Marker','s',line_opts{:});

    ypos = 0.15; text(0.2,ypos,'VS-Lite-like',format2{:});
    line([0.1 0.1],[ypos ypos],'Marker','^',line_opts{:});

    ypos = 0.05; text(0.2,ypos,'VS-Lite smooth',format2{:});
    line([0.1 0.1],[ypos ypos],'Marker','o',line_opts{:});

    export_fig('-transparent',[prefix '.pdf'],figure_hdl);
    %     export_fig('-transparent','-nocrop',[prefix '.eps'],figure_hdl);
    saveas(figure_hdl,[prefix '.fig'],'fig');
end
				      
					      