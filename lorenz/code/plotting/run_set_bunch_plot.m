function run_set_bunch_plot(cfg)
    print_function_message('opening')

    cfg.Tkind         = {'Insta','Taver','Tanom'};
    cfg.Ephase        = {'prior','postr'};
    cfg.var_name  = {'comp1','comp2'};
    cfg.var_label = {'Fast Component','Slow Component'};
    cfg.plot_formats  = {'-pdf','-eps'};
    cfg.free_run_marker = 's';


    switch cfg.set_dim
        case 1
            set1D_plots(cfg)
        case 2
            set2D_plots(cfg)
    end
    
    print_function_message('closing')
end

function set2D_plots(cfg)
    cstat     = cfg.diag{:};
    cfg.cstat = cfg.diag{1};
    
%    for l=1:length(cfg.run_set_names)
    for l=1:5
         run_set_error_surf_plot_2comp(cfg,cstat,cfg.run_set_names{l});
         run_set_error_reduc_surf_plot_2comp(cfg,cstat,cfg.run_set_names{l});
    end
end

function run_set_error_surf_plot_2comp(cfg,cstat,run_set_name)
    print_function_message('opening')

    error_stat = 'error_Tstdd';
%     plot_name  = [ error_stat '_surfs'];
   plot_name  = [ error_stat '_surf__' run_set_name];
            
    % Import data
%     cfg

    Tkind = 'Taver';
    Ephase= 'postr';
    
    %     run_set_name =cfg.run_set_names{2};
    stat_path = ['../run_sets/' run_set_name '/stats/' 'assi_'...
                error_stat '_' cstat '_' Ephase '_' Tkind '.nc']
    assi_error = get_scalar_netcdf(cfg,stat_path);
            
    % Create surf plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 700 300]); clf;

    plot_width    = 0.36 ;   plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = 0.1 + [0  plot_width + 0.08];

    color_axis = {[1.2 3.2],[0.3 1.3]};

    for l=1:2
 %       subplot(1,2,l);
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        imagesc(assi_error.dim(1).data,assi_error.dim(2).data,assi_error.var(l).data');
        set(gca,'YDir','normal');
        axis tight;

        if isequal(cfg.set_par1_name,'range_upper')
            isequal(l,2) && ylabel('R^B','FontSize', 20,'rot',0);
            ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
        else
            isequal(l,1) && ylabel(assi_error.dim(2).label,'FontSize', 20);
        end
        
        if isequal(cfg.set_par2_name,'range_lower')
            xlabel('R^A','FontSize', 20);
            xlim_curr = get(gca,'xlim'); set(gca, 'xlim', [0 xlim_curr(2)]);
        else
            xlabel(assi_error.dim(1).label,'FontSize', 20);
        end
        
        %         if isequal(l,1)
%             xlabel('R^A','FontSize', 20); ylabel('R^B','FontSize', 20,'rot',90);
%         else
%             xlabel('M^B','FontSize', 20); ylabel('M^A','FontSize', 20,'rot',90);
%         end
        %isequal(l,1) && ylabel(assi_error.dim(2).label,'FontSize', 20);
        
        hcb = colorbar; 
        caxis(color_axis{l});
        if isequal(l,1)
            set(hcb,'YTick',...
                (floor(color_axis{l}(1)*10):2:floor(color_axis{l}(2)*10))/10);
        else
            set(hcb,'YTick',...
                (floor(color_axis{l}(1)*10):floor(color_axis{l}(2)*10))/10);
        end

        %         hcbt = 
%         set(hcb,'YTick',...
%            (floor(color_axis{l}(1)*10):floor(color_axis{l}(2)*10):2)/10);
%         set(hcb,'YTickLabel',sprintf('%.1f |',hcbt'));

%         isequal(l,2) && ylabel(hcb,'RMS Error','FontSize', 20);

        title(cfg.var_label{l},'FontSize', 20);
        set(gca,'YTick',floor(min(assi_error.dim(2).data):ceil(max(assi_error.dim(2).data))));

        %         n=get(gca,'xtick');
%         set(gca,'xticklabel',sprintf('%.1f |',n'));

%                 plot(assi_error{j,k}.dim.data,assi_error{j,k}.var(l).data,...
%                     'LineStyle','-','Color',line_color,'Marker',marker_shape,...
%                     'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
    end
%     mtit(label(cfg.bunch_par1_values{2},'obs_operator'));
        
    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);
            
%         cfg.Tkind = {'Insta','Taver'};
%     for j=1:length(cfg.Tkind)
%         Tkind = cfg.Tkind{j};
% 
% %         free_error{j}  = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
% %             '/stats/' 'free_'  error_stat '_' cstat '_prior'   '_' Tkind '.nc']);
%     
%         for k=1:length(cfg.Ephase)
%             Ephase = cfg.Ephase{k};
%             
%             assi_error {j,k}  = get_scalar_netcdf(cfg,...
%                 ['../run_sets/' run_set_name '/stats/' 'assi_'...
%                 error_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
%         end
%     end
%     
%     % Create line plots
% 
%     figure_hdl=figure();
%     set(figure_hdl,'Name',plot_name,'NumberTitle','off');
%     set(figure_hdl,'Position', [0 0 800 300]); clf;
% 
%     plot_width    = 0.29 ;   plot_height = 0.74 ;
%     plot_ypos     = 0.14;   plot_xpos   = 0.1 + [0  plot_width + 0.04];
% 
%     for j=1:1%length(cfg.Tkind)
% %             par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
%     
%         for k=1:1%length(cfg.Ephase)
% %                 par2.value_label{k} = label(cfg.Ephase{k},'Ephase');
%             for l=1:2
%                 subplot(1,2,l);
%                 
% %                 axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
%                 imagesc(assi_error{j,k}.var(l).data);
%                 axis tight;
%                 xlabel(assi_error{j,k}.dim(1).label,'Interpreter','Tex');
%                 ylabel(assi_error{j,k}.dim(2).label);
%                 colorbar;
% 
%             
% %                 plot(assi_error{j,k}.dim.data,assi_error{j,k}.var(l).data,...
% %                     'LineStyle','-','Color',line_color,'Marker',marker_shape,...
% %                     'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
%             end
%             
% %             plot(free_error{j}.dim.data,free_error{j}.var(l).data,...
% %                 'LineStyle','-','Color',line_color,'Marker',marker_shape,...
% %                 'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
% %             par2.value_label{3} = 'Free Run';
%             
%         end
% %         if(l==1); ylabel(label(error_stat,'stat')); end
%     end
    print_function_message('closing')
end

function run_set_error_reduc_surf_plot_2comp(cfg,cstat,run_set_name)
    print_function_message('opening')

    error_stat = 'error_Tstdd_reduc';
%     plot_name  = [ error_stat '_surfs'];
    plot_name  = [ error_stat '_surf__' run_set_name];
            
    % Import data
    Tkind = 'Taver';
    Ephase= 'postr';
    
    %     run_set_name =cfg.run_set_names{2};
    stat_path = ['../run_sets/' run_set_name '/stats/' 'assi_'...
                error_stat '_' cstat '_' Ephase '_' Tkind '.nc']
    assi_error = get_scalar_netcdf(cfg,stat_path);
            
    % Create surf plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.3;   plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.03];

    color_axis = {[-100 100],[-100 100]};

    for l=1:2
 %       subplot(1,2,l);
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        imagesc(assi_error.dim(1).data,assi_error.dim(2).data,assi_error.var(l).data');
        set(gca,'YDir','normal');
        axis tight;
        
%         xlabel(cfg.set_par1_label{:}); ylabel(cfg.set_par2_label{:});
        xlabel(assi_error.dim(1).label); 
        
%        isequal(l,1) && ylabel(assi_error.dim(2).label);
        
%         if isequal(l,1)
%             xlabel('T^B','FontSize', 20); ylabel('T^A','FontSize', 20,'rot',90);
%         else
%             xlabel('M^B','FontSize', 20); ylabel('M^A','FontSize', 20,'rot',90);
%         end
%            ylabel(assi_error.dim(2).label,'FontSize', 20);
%        xlabel(assi_error.dim(1).label,'FontSize', 20);
        %isequal(l,1) && ylabel(assi_error.dim(2).label,'FontSize', 20);
        
%         hcbt = 
%         set(hcb,'YTick',...
%            (floor(color_axis{l}(1)*10):floor(color_axis{l}(2)*10))/10);
%         set(hcb,'YTickLabel',sprintf('%.1f |',hcbt'));

            %         c_map = colormap(othercolor('Spectral10'));

        axis tight; 
        if isequal(cfg.set_par1_name,'range_upper')
            isequal(l,1) && ylabel('R^B','FontSize', 20,'rot',0);
            ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
            isequal(l,1) && set(gca,'YTick',floor(min(assi_error.dim(2).data):ceil(max(assi_error.dim(2).data))));
        else
            isequal(l,1) && ylabel(assi_error.dim(2).label,'FontSize', 20);
        end
        
        if isequal(cfg.set_par2_name,'range_lower')
            xlabel('R^A','FontSize', 20);
            xlim_curr = get(gca,'xlim'); set(gca, 'xlim', [0 xlim_curr(2)]);
        else
            xlabel(assi_error.dim(1).label,'FontSize', 20);
        end
        
%         xlim_curr = get(gca,'xlim'); set(gca, 'xlim', [0 xlim_curr(2)]);
%         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
            
        c_map = colormap(othercolor('BuOr_12'));
        colormap(flipud(c_map));
        caxis(color_axis{l});
 
        if isequal(l,2)
            hcb = colorbar;
            ylabel(hcb,'Error Reduction (%)','FontSize', 20);
        end

        title(cfg.var_label{l},'FontSize', 16);

        %         n=get(gca,'xtick');
%         set(gca,'xticklabel',sprintf('%.1f |',n'));

%                 plot(assi_error{j,k}.dim.data,assi_error{j,k}.var(l).data,...
%                     'LineStyle','-','Color',line_color,'Marker',marker_shape,...
%                     'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
    end
    
    % Colorbar customization
    p = get(hcb,'Position');  
    p(1) = 0.75; p(2) = 0.14; p(4) = 0.74;
    set(hcb,'Position',p);
        
    % save figure
    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);
            
    print_function_message('closing')
end


function set1D_plots(cfg)
    print_function_message('opening')

    cfg.run_set_names
    % Add plot configuration data
    
    cfg.legend_width  = 0.2;
    cfg.marker_center = 0.13;
    cfg.label_start   = 0.26;
    cfg.line_space    = 0.07;
    cfg.line_width    = 0.16;
    cfg.plot_formats  = {'-pdf','-eps'};
    
    
    cfg.format1       = {'HorizontalAlignment','left','FontSize',14,'FontWeight','bold'};
    cfg.format2       = {'HorizontalAlignment','left','FontSize',14};
    cfg.line_opts     = {'Color','k','LineWidth',2,'LineStyle','-','MarkerSize',8};
    cfg.marker_shapes = {'s','o','^','v','x'};
    cfg.line_styles   = {':','-',':'};
%     cfg.line_colors   = {'r','b','m','g','c'};
    cfg.line_colors   = {'r','c','b','g','m','c'};
%     cfg.line_colors   = {'c','m','r',  'b','g'};
%     cfg.marker_shapes = {'+', 's', '^', 'v', 'o'};


%     cfg.par_value_to_miss.update_mode ='Augm3';
%     cfg.par_value_to_miss.obs_operator ='addi';
%     cfg.par_value_to_miss.update_mode ='';

    Tkind  = {'Insta','Taver','Tanom'};
    Ephase = {'prior','postr'};
    cstat = cfg.diag{:};
    cfg.cstat = cfg.diag{1};
    %'Fmean';
    %    cstat = 'Xsel';
    
%     covar_nature_Taver__obs_clean_Taver__plot(cfg);
    %corr_nature_Taver__obs_clean_Taver__plot(cfg);

    nature_Tstdd_plot_2comp(cfg);
    
    for l=1:length(cfg.run_set_names)
%    for l=1:1
         run_set_error_line_plot_2comp(cfg,cstat,cfg.run_set_names{l});
    end
    
    for j=1:length(Tkind)
%         error_by_Ephase_line_plot_2comp(cfg,cstat,Tkind{j})
        for k=1:length(Ephase)
            
            error_line_plot_2comp(cfg,cstat,Ephase{k},Tkind{j});
            if isequal(cfg.bunch_par1_name{:},'obs_operator')
                error_increase_line_plot_2comp(cfg,cstat,Ephase{k},Tkind{j});
            end
            
            error_spread_line_plot_2comp(cfg,cstat,Ephase{k},Tkind{j});
            standard_dev_line_plot_2comp(cfg,cstat,Ephase{k},Tkind{j});
%             
%             stat = {'error_Tstdd_reduc','Esprd_Tmean_reduc','Emean_Tstdd_reduc'};
            stat = {'error_Tstdd_reduc'};
            for l=1:length(stat)
                scalar = ['assi_' stat{l} '_' cstat '_' Ephase{k} '_' Tkind{j}]
            
                stat_reduction_line_plot_2comp(cfg,scalar);
            
            end
        end
    end
    print_function_message('closing')
end

% Line plots

function nature_Tstdd_plot_2comp(cfg)
    print_function_message('opening')

    nature_stat  = 'nature_Tstdd';
    plot_name  = [ nature_stat '__plot'];
            
    % Import data

    for j=1:length(cfg.Tkind)
        Tkind = cfg.Tkind{j};
        
        netcdf_path=['../' cfg.rel_run_free_set_dir{:} ...
            '/stats/' nature_stat '_' cfg.cstat '_' Tkind '.nc'];
%         system(['ls ../' cfg.rel_run_free_set_dir{:} '/stats']);
%         system(['ncdump -h ' netcdf_path]);
        nature_stdd.(Tkind) = get_scalar_netcdf(cfg, netcdf_path);
    end
    
    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.29 ;   plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = 0.1 + [0  plot_width + 0.04];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;
        
        for j=1:length(cfg.Tkind)
            Tkind = cfg.Tkind{j};
            
            par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
            line_color = cfg.line_colors{j};
            par1.colors{j} = line_color;
            marker_shape = cfg.marker_shapes{j};            
            par1.markers{j} = marker_shape;
    
            plot(nature_stdd.(Tkind).dim.data,nature_stdd.(Tkind).var(l).data,...
                    'LineStyle','-','Color',line_color,'Marker',marker_shape,...
                    'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);

          
        end

        xlabel(nature_stdd.(Tkind).dim.label);  
        if(l==1); ylabel(label(nature_stat,'stat')); end
        axis tight; 
%         ylim([0 12]);
         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)*1.05]);
 
        title(nature_stdd.(Tkind).var(l).label);
    end

    % Add Legends
    
    cfg.legend_width  = 0.25;  
%     legend(par1.value_label{1:end},'Location','EastOutside');
    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
    par1.label=''; 
    corner_pos =  color_legend(cfg,corner_pos, par1);
%     corner_pos = marker_legend(cfg,corner_pos, par2);

    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);
    print_function_message('closing')
end

function run_set_error_line_plot_2comp(cfg,cstat,run_set_name)
    print_function_message('opening')

    error_stat = 'error_Tstdd';
    plot_name  = [ error_stat '__' run_set_name];
    cfg.line_colors   = {'r','b','g'};
    cfg.marker_shapes = {'>','o',cfg.free_run_marker};
            
    % Import data

    cfg.Tkind = {'Insta','Taver'};
    for j=1:length(cfg.Tkind)
        Tkind = cfg.Tkind{j};

        free_error{j}  = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
            '/stats/' 'free_'  error_stat '_' cstat '_prior'   '_' Tkind '.nc']);
    
        for k=1:length(cfg.Ephase)
            Ephase = cfg.Ephase{k};
            
            assi_error {j,k}  = get_scalar_netcdf(cfg,['../run_sets/' run_set_name ...
          '/stats/' 'assi_'  error_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
        end
    end
    
    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.29 ;   plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = 0.1 + [0  plot_width + 0.05];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;
        
        for j=1:length(cfg.Tkind)
            par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
            line_color = cfg.line_colors{j};
            line_style = cfg.line_styles{j};
            par1.colors{j} = line_color;
            par1.styles{j} = line_style;
    
            for k=1:length(cfg.Ephase)
                par2.value_label{k} = label(cfg.Ephase{k},'Ephase');
                marker_shape = cfg.marker_shapes{k};
            
                plot(assi_error{j,k}.dim.data,assi_error{j,k}.var(l).data,...
                    'LineStyle',line_style,'Color',line_color,'Marker',marker_shape,...
                    'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
            end
            
            marker_shape = cfg.marker_shapes{3};
            plot(free_error{j}.dim.data,free_error{j}.var(l).data,...
                'LineStyle',line_style,'Color',line_color,'Marker',marker_shape,...
                'MarkerSize',6,'Linewidth',1,'MarkerEdgeColor',line_color);
            par2.value_label{3} = 'Free Run';
            
        end

        xlabel(free_error{j}.dim.label);  
        if(l==1); ylabel(label(error_stat,'stat')); end
        axis tight; 
        ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)*1.05]);

        title(free_error{j}.var(l).label);
    end

    % Add Legends
    
    cfg.legend_width  = 0.23;    
    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
    par1.label=''; par2.label='';
%     lineStyle_legend(cfg,corner_pos, par)
    corner_pos = color_Style_legend(cfg,corner_pos, par1);
%     corner_pos =  color_legend(cfg,corner_pos, par1);
    corner_pos =      marker_legend(cfg,corner_pos, par2);

    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function covar_nature_Taver__obs_clean_Taver__plot(cfg)
    print_function_message('opening')

    plot_name  = 'covar_nature__obs_clean_Taver__plot';
           
    % Import data
    
    for j=1:length(cfg.Tkind)
        Tkind = cfg.Tkind{j};
        stat = ['covar_nature_' Tkind '__obs_clean_Taver'];
        netcdf_path = ['../run_sets/' cfg.run_set_names{1} '/stats/' stat '.nc'];
        if ~ exist(netcdf_path,'file'); return; end
        covar_obs_nature {j} = get_scalar_netcdf(cfg,netcdf_path);
        par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
    end

    
%     strucdisp(covar_obs_nature);

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 400]); clf;

    plot_width    = 0.5 ;  plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.15 ;

    axes('pos',[plot_xpos plot_ypos plot_width plot_height]);
    hold on;

    for l=1:2
        marker_shape        = cfg.marker_shapes{l};
        par2.value_label{l} = cfg.var_label{l};
        par2.markers    {l} = marker_shape;
        
%             bunch_par1.value_label{j1} = ...
%                 label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
        for j=1:length(cfg.Tkind)
            line_color          = cfg.line_colors{j};
            par1.colors {j} = line_color;

            plot(covar_obs_nature{j}.dim.data,covar_obs_nature{j}.var(l).data,...
            'LineStyle','-','Color',line_color,'Marker',marker_shape,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        
%         for k=1:length(cfg.run_set_names)
%             j1 = cfg.bunch_par1_value_pos(k)+1; 
%             
%             if isfield(cfg,'par_value_to_miss')
%                 if isequal(cfg.bunch_par1_values{j1},...
%                         cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
%                     continue
%                 end
%             end
%             
%             line_color   =   cfg.line_colors{j1};
%             

%             bunch_par1.colors{j1} = line_color;
%             
%             if(cfg.bunch_dim == 1)
%                 marker_shape = cfg.marker_shapes{j1};
%                 bunch_par1.markers{j1} = marker_shape;
%             else
%                 j2 = cfg.bunch_par2_value_pos(k)+1;
%                 marker_shape = cfg.marker_shapes{j2};
% 
%                 bunch_par2.value_label{j2} = ...
%                     label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
%             end
        end
    end
    xlabel(covar_obs_nature{j}.dim.label);
    ylabel('Pearson Correlation');

    axis tight; 
%         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [ylim_curr(1) 1.01]);
%     ylim([-0.41, 1.01]);
%     title(covar_obs_nature{j}.var(l).label);
    hline = refline([0 0]);
    set(hline,'Color','k');

    % Add Legends
    
    cfg.legend_width  = 0.32;    
    corner_pos = [(plot_xpos + plot_width + 0.02) (plot_ypos + plot_height)];
    par1.label = label('Tkind','par');
    corner_pos =  color_legend(cfg,corner_pos, par1);
    par2.label = '';
    corner_pos = marker_legend(cfg,corner_pos, par2);
    
%     corner_pos = reference_run_legend(cfg,corner_pos,'Free Run');
% %   legend(bunch_par1.value_label{1:end},'Location','EastOutside');
% 
    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function corr_nature_Taver__obs_clean_Taver__plot(cfg)
    print_function_message('opening')

    plot_name  = 'corr_nature__obs_clean_Taver__plot';
           
    % Import data
    
    for j=1:length(cfg.Tkind)
        Tkind = cfg.Tkind{j};
        stat = ['corr_nature_' Tkind '__obs_clean_Taver'];
        netcdf_path = ['../run_sets/' cfg.run_set_names{1} '/stats/' stat '.nc'];
        
        corr_obs_nature {j} = get_scalar_netcdf(cfg,netcdf_path);
        par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
    end

    
%     strucdisp(corr_obs_nature);

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 400]); clf;

    plot_width    = 0.5 ;  plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.15 ;

    axes('pos',[plot_xpos plot_ypos plot_width plot_height]);
    hold on;

    for l=1:2
        marker_shape        = cfg.marker_shapes{l};
        par2.value_label{l} = cfg.var_label{l};
        par2.markers    {l} = marker_shape;
        
%             bunch_par1.value_label{j1} = ...
%                 label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
        for j=1:length(cfg.Tkind)
            line_color          = cfg.line_colors{j};
            par1.colors {j} = line_color;

            plot(corr_obs_nature{j}.dim.data,corr_obs_nature{j}.var(l).data,...
            'LineStyle','-','Color',line_color,'Marker',marker_shape,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        
%         for k=1:length(cfg.run_set_names)
%             j1 = cfg.bunch_par1_value_pos(k)+1; 
%             
%             if isfield(cfg,'par_value_to_miss')
%                 if isequal(cfg.bunch_par1_values{j1},...
%                         cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
%                     continue
%                 end
%             end
%             
%             line_color   =   cfg.line_colors{j1};
%             

%             bunch_par1.colors{j1} = line_color;
%             
%             if(cfg.bunch_dim == 1)
%                 marker_shape = cfg.marker_shapes{j1};
%                 bunch_par1.markers{j1} = marker_shape;
%             else
%                 j2 = cfg.bunch_par2_value_pos(k)+1;
%                 marker_shape = cfg.marker_shapes{j2};
% 
%                 bunch_par2.value_label{j2} = ...
%                     label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
%             end
        end
    end
    xlabel(corr_obs_nature{j}.dim.label);
    ylabel('Pearson Correlation');

    axis tight; 
%         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [ylim_curr(1) 1.01]);
    ylim([-0.41, 1.01]);
%     title(corr_obs_nature{j}.var(l).label);
    hline = refline([0 0]);
    set(hline,'Color','k');

    % Add Legends
    
    cfg.legend_width  = 0.32;    
    corner_pos = [(plot_xpos + plot_width + 0.02) (plot_ypos + plot_height)];
    par1.label = label('Tkind','par');
    corner_pos =  color_legend(cfg,corner_pos, par1);
    par2.label = '';
    corner_pos = marker_legend(cfg,corner_pos, par2);
    
%     corner_pos = reference_run_legend(cfg,corner_pos,'Free Run');
% %   legend(bunch_par1.value_label{1:end},'Location','EastOutside');
% 
    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function error_increase_line_plot_2comp(cfg,cstat,Ephase,Tkind)
    print_function_message('opening')

    plot_name  = ['error-increase_plot_' Ephase '_' Tkind];
    error_stat =  'error_Tstdd';
    
    cfg.line_colors   = {'k','r','b','g','m','c'};
            
    % Import data
    
    free_error  = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
      '/stats/' 'free_'  error_stat '_' cstat '_prior'   '_' Tkind '.nc']);
    for k=1:length(cfg.run_set_names)
        assi_error (k)  = ...
            get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' 'assi_'  error_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
    end

    for l=1:2
        for k=1:length(cfg.run_set_names)
            error_increase {k}{l}  = 100 * ...
              ((assi_error(k).var(l).data ./ assi_error(1).var(l).data)-1);
        end
    end
            
    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 750 350]); clf;
%     set(figure_hdl,'Position', [0 0 650 300]); clf;

    plot_width    = 0.3 ;  plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.01];

    for l=1:2
        hl(l)=axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1; 
                        
            if isequal(cfg.bunch_par1_values{j1},'Resp_add'); 
               continue
            end
            
            line_color   =   cfg.line_colors{j1};
            
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors{j1} = line_color;
            
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                marker_shape = cfg.marker_shapes{j2};

                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end

            plot(assi_error(k).dim.data, error_increase{k}{l},...
                'LineStyle','-','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end
        
        xlabel(free_error.dim.label);  
        if(l==1); ylabel('Error Increase (%)'); end
        if(l==2); set(gca, 'YTickLabel', []); end

        axis tight;
%         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [-2 ylim_curr(2)]);
        ylim([-2 50]);

        title(free_error.var(l).label);
    end
    
%    linkaxes(hl(l),'y');

    % Add Legends
    cfg.legend_width  = 0.16; 
    corner_pos = [(plot_xpos(1) + 0.13) (plot_ypos + plot_height-0.02)];
%     corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
    bunch_par1.label = 'T-norm';
%     bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos =  color_legend(cfg,corner_pos, bunch_par1);
    if (cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
        corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
    end

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function error_line_plot_2comp(cfg,cstat,Ephase,Tkind)
    print_function_message('opening')

    plot_name  = ['error-plot_' Ephase '_' Tkind];
    error_stat =  'error_Tstdd';
    cfg.marker_shapes = {'o','s','^','v','x'};
    cfg.line_styles   = {'-',':',':'};
%     cfg.line_colors   = {'r','b','m','g','c'};
    cfg.line_colors   = {'r','c','b','g','m','c'};

            
    % Import data
    
    free_error  = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
      '/stats/' 'free_'  error_stat '_' cstat '_prior'   '_' Tkind '.nc']);
    for k=1:length(cfg.run_set_names)
        assi_error (k)  = get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' 'assi_'  error_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
    end

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 650 300]); clf;

    plot_width    = 0.28 ;  plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.05];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1; 
            
            extract_values='yes';
            if isequal(extract_values,'yes')
                if isequal(k,2)||isequal(k,6)
                    continue
                end
            end
%             if isfield(cfg,'par_value_to_miss')
%                 if isequal(cfg.bunch_par1_values{j1},...
%                         cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
%                     continue
%                 end
%             end
            
            line_color   =   cfg.line_colors{j1};
            marker_shape = cfg.marker_shapes{j1};
            
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors {j1} = line_color;
            bunch_par1.markers{j1} = marker_shape;
            
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
                LineStyle = cfg.line_styles{1};
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                LineStyle = cfg.line_styles{j2};

                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end

            plot(assi_error(k).dim.data,assi_error(k).var(l).data,...
                'LineStyle',LineStyle,'Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end

        plot(free_error.dim.data,free_error.var(l).data,...
            'LineStyle','-','Color','k','Marker',cfg.free_run_marker,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor','k');
        
        xlabel(free_error.dim.label);  
        if(l==1); ylabel('RMSE'); end

        axis tight; 
        ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);

        title(free_error.var(l).label);
    end

    % Add Legends
    cfg.legend_width  = 0.25; 
    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
    bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos =  color_legend(cfg,corner_pos, bunch_par1);
    if (cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
%         corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
        corner_pos = lineStyle_legend(cfg,corner_pos, bunch_par2);
    end
    corner_pos = reference_run_legend(cfg,corner_pos,'Free Run');
%   legend(bunch_par1.value_label{1:end},'Location','EastOutside');

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function error_spread_line_plot_2comp(cfg,cstat,Ephase,Tkind)
    print_function_message('opening')

    plot_name    = ['error-spread_plot_' Ephase '_' Tkind];
    error_stat   = 'error_Tstdd';
    spread_stat  = 'Esprd_Tmean';
    cfg.legend_width  = 0.2;
            
    % Import data
    
    free_error  = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
      '/stats/' 'free_'  error_stat '_' cstat '_prior'   '_' Tkind '.nc']);
    free_spread = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
      '/stats/' 'free_' spread_stat '_' cstat '_prior'   '_' Tkind '.nc']);
    for k=1:length(cfg.run_set_names)
        assi_error (k)  = get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' 'assi_'  error_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
        assi_spread(k)  = get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' 'assi_' spread_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
    end

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.3;   plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.04];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        plot(free_error.dim.data,free_error.var(l).data,...
            'LineStyle','-','Color','k','Marker',cfg.free_run_marker,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor','k');
        plot(free_spread.dim.data,free_spread.var(l).data,...
            'LineStyle',':','Color','k','Marker',cfg.free_run_marker,...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor','k');

        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1;

            if isfield('cfg','par_value_to_miss')
                if isequal(cfg.bunch_par1_values{j1},...
                        cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
                    continue
                end
            end
            
            line_color = cfg.line_colors{j1};
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors{j1} = line_color;
            
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                marker_shape = cfg.marker_shapes{j2};
                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end

            plot(assi_error(k).dim.data,assi_error(k).var(l).data,...
                'LineStyle','-','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
            plot(assi_spread(k).dim.data,assi_spread(k).var(l).data,...
                'LineStyle',':','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end

        xlabel(free_error.dim.label);  
        %if(l==1); ylabel(label(assi_scalar,'stat')); end
        axis tight; 
        ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);

        title(free_error.var(l).label);
    end

    % Add Legends
    
    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)]; 
%     cfg.legend_width  = 0.26;


    bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos = color_legend(cfg,corner_pos,bunch_par1);
    
    if(cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
        corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
    end
    
    corner_pos = error_spread_legend (cfg,corner_pos);
    corner_pos = reference_run_legend(cfg,corner_pos,'Free ensemble');

    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function error_by_Ephase_line_plot_2comp(cfg,cstat,Tkind)
    print_function_message('opening')
    
    disp('!!!! Unfinished plotting routine')

    plot_name    = ['error_by_Ephase__plot_' Tkind];
    error_stat   = 'error_Tstdd';
%     spread_stat  = 'Esprd_Tmean';
            
    % Import data
    
    for k=1:length(cfg.run_set_names)
        nc_path=['../run_sets/' cfg.run_set_names{k} '/stats/' 'assi_' ...
            error_stat '_' cstat '_' 'prior' '_' Tkind '.nc'];
        assi_error_prior (k)  = get_scalar_netcdf(cfg,nc_path);
        nc_path=['../run_sets/' cfg.run_set_names{k} '/stats/' 'assi_' ...
            error_stat '_' cstat '_' 'postr' '_' Tkind '.nc'];
        assi_error_postr(k)  = get_scalar_netcdf(cfg,nc_path);
    end

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.3;   plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.04];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1;

            if isfield('cfg','par_value_to_miss')
                if isequal(cfg.bunch_par1_values{j1},...
                        cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
                    continue
                end
            end
            
            line_color = cfg.line_colors{j1};
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors{j1} = line_color;
            
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                marker_shape = cfg.marker_shapes{j2};
                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end

            plot(assi_error_prior(k).dim.data,assi_error_prior(k).var(l).data,...
                'LineStyle','-','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
            plot(assi_error_postr(k).dim.data,assi_error_postr(k).var(l).data,...
                'LineStyle',':','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end

        xlabel(assi_error_prior(1).dim.label);  
        %if(l==1); ylabel(label(assi_scalar,'stat')); end
        axis tight; 
        ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);

        title(assi_error_prior(1).var(l).label);
    end

    % Add Legends
    
    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)]; 

    bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos = color_legend(cfg,corner_pos,bunch_par1);
    
    if(cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
        corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
    end
    Ephase.label='';
    Ephase.value_label={'Analysis','Forescast'};
    corner_pos = lineStyle_legend(cfg,corner_pos, Ephase);
%     corner_pos = error_spread_legend (cfg,corner_pos);
    corner_pos = reference_run_legend(cfg,corner_pos,'Free ensemble');

    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function standard_dev_line_plot_2comp(cfg,cstat,Ephase,Tkind)
    print_function_message('opening')

    plot_name   = ['state_std_dev_plot_' Ephase '_' Tkind];
    assi_stat    =  'Emean_Tstdd';
    nature_stat  = 'nature_Tstdd';
            
    % Import data
    
    nature_stdd = get_scalar_netcdf(cfg,['../' cfg.rel_run_free_set_dir{:} ...
      '/stats/' nature_stat '_' cstat '_' Tkind '.nc']);
    for k=1:length(cfg.run_set_names)
        assi_stdd (k)  = get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' 'assi_' assi_stat '_' cstat '_' Ephase '_' Tkind '.nc']);
    end

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); clf;

    plot_width    = 0.3;   plot_height = 0.74 ;
    plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.04];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        plot(nature_stdd.dim.data,nature_stdd.var(l).data,...
            'LineStyle','-','Color','k','Marker','d',...
            'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor','k');
        
        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1; 
            
%             if isfield(cfg,'par_value_to_miss')
%                 if isequal(cfg.bunch_par1_values{j1},...
%                         cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
%                     continue
%                 end
%             end
            
            line_color   =   cfg.line_colors{j1};
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors{j1} = line_color;
            
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                marker_shape = cfg.marker_shapes{j2};
                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end
            
            plot(assi_stdd(k).dim.data,assi_stdd(k).var(l).data,...
                'LineStyle','-','Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end

        xlabel(nature_stdd.dim.label);  
        if(l==1); ylabel('State Std. Dev.'); end
        axis tight; 
        ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);

        title(nature_stdd.var(l).label);
    end

    % Add Legends

    corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
    bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos =  color_legend(cfg,corner_pos, bunch_par1);
    if (cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
        corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
    end
    corner_pos = reference_run_legend(cfg,corner_pos,'Nature run');

    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

function stat_reduction_line_plot_2comp(cfg,scalar)
    print_function_message('opening')

    cfg.marker_shapes = {'o','s','<','>','^','v','x'};
    cfg.line_styles   = {':','-'};
%     cfg.line_colors   = {'r','b','m','g','c'};
    cfg.line_colors   = {'r','c','b','g','m','c'};
    cfg.legend_width  = 0.28; 
    
    % Import data
    
    plot_name = scalar;
    for k=1:length(cfg.run_set_names)
        assi_scalar(k) = get_scalar_netcdf(cfg,['../run_sets/' cfg.run_set_names{k} ...
          '/stats/' scalar '.nc']);
    end

    % Create line plots

    figure_hdl=figure();
    set(figure_hdl,'Name',plot_name,'NumberTitle','off');
    set(figure_hdl,'Position', [0 0 600 300]); 

    plot_width    = 0.29;   plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = 0.1 + [0  plot_width + 0.01];

    for l=1:2
        axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
        hold on;

        for k=1:length(cfg.run_set_names)
            j1 = cfg.bunch_par1_value_pos(k)+1; 
            
            extract_values='yes';
            if isequal(extract_values,'yes')
                if isequal(k,2)||isequal(k,6)
                    continue
                end
            end

            line_color   =   cfg.line_colors{j1};
            marker_shape = cfg.marker_shapes{j1};
            
            bunch_par1.value_label{j1} = ...
                label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
            bunch_par1.colors {j1} = line_color;
            bunch_par1.markers{j1} = marker_shape;
           
            if(cfg.bunch_dim == 1)
                marker_shape = cfg.marker_shapes{j1};
                bunch_par1.markers{j1} = marker_shape;
                LineStyle = cfg.line_styles{1};
            else
                j2 = cfg.bunch_par2_value_pos(k)+1;
                LineStyle = cfg.line_styles{j2};

%                 marker_shape = cfg.marker_shapes{j2};
                bunch_par2.value_label{j2} = ...
                    label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
            end
            
            plot(assi_scalar(k).dim.data,assi_scalar(k).var(l).data,...
                'LineStyle',LineStyle,'Color',line_color,'Marker',marker_shape,...
                'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
        end

        xlabel(assi_scalar(1).dim.label);  
        if(l==1); ylabel(label(scalar,'stat')); end
        axis tight;
        
        ylim([-10 100])
%         ylim_curr = get(gca,'ylim');
%         if (ylim_curr(2) < 0.0); ylim auto
%         else                     ylim([0 100])
%         end
        
        if(l==2); set(gca, 'YTickLabel', []); end
%         if((l==2)&&(cfg.bunch_dim == 1))
%             legend(par_labels{1:end},'Location','EastOutside');
%         end
                     
        title(assi_scalar(1).var(l).label);
    end
    
    % Add Legends
    
    corner_pos = [(plot_xpos(2) + plot_width + 0.01) (plot_ypos + plot_height)];
    bunch_par1.label = label(cfg.bunch_par1_name{:},'par');
    corner_pos =  color_legend(cfg,corner_pos, bunch_par1);
    if (cfg.bunch_dim == 2)
        bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
        corner_pos = lineStyle_legend(cfg,corner_pos, bunch_par2);
%         corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
    end
%   legend(bunch_par1.value_label{1:end},'Location','EastOutside');
    
    % save figure

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);

    close(figure_hdl);

    print_function_message('closing')
end

% Legend boxes

function corner_pos = color_legend(cfg,corner_pos,par)
    
     par.value_label = par.value_label(~cellfun('isempty',par.value_label));
     par.colors      = par.colors     (~cellfun('isempty',par.colors));
     if isfield(par,'markers')
         par.markers      = par.markers (~cellfun('isempty',par.markers));
     end

    if (~ isequal(par.label,''))
        box_height = cfg.line_space * (length(par.value_label)+1);
    else
        box_height = cfg.line_space * length(par.value_label);
    end
    
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2;

    if (~ isequal(par.label,''))
        text(0.1,ypos, par.label,cfg.format1{:});
        ypos = ypos - cfg.line_space;
    end
    
    for j =1:length(par.value_label)
        line(cfg.marker_center + cfg.line_width *[-0.5 0.5],[ypos ypos],...
            'Color',par.colors{j},'LineStyle','-','LineWidth',1);
%             'Color',par.colors{j},'LineWidth',2,'LineStyle','-');
        if isfield(par,'markers')
            line(cfg.marker_center*[1 1],[ypos ypos],'Color',par.colors{j},...
                'Marker',par.markers{j},'MarkerEdgeColor',par.colors{j},...
                'LineWidth',1,'MarkerSize',7);
%                 'MarkerEdgeColor',par.colors{j},'LineWidth',2,'MarkerSize',7,'Color','w');
        end
        text(cfg.marker_center + 0.5*cfg.line_width +0.05,ypos,...
            par.value_label{j},cfg.format2{:},'Color',par.colors{j});
        ypos = ypos - cfg.line_space; 
    end
    
    corner_pos(2) = corner_pos(2) - box_height;
end

function corner_pos = marker_legend(cfg,corner_pos, par)

     par.value_label = par.value_label(~cellfun('isempty',par.value_label));
%      par.colors      = par.colors     (~cellfun('isempty',par.colors));

     if (~ isequal(par.label,''))
        box_height = cfg.line_space * (length(par.value_label)+1);
    else
        box_height = cfg.line_space *  length(par.value_label);
    end
    
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2;

    if (~ isequal(par.label,''))
        text(0.1,ypos, par.label,cfg.format1{:});
        ypos = ypos - cfg.line_space;
    end
    
    for j =1:length(par.value_label)
        line(cfg.marker_center*[1 1],[ypos ypos],...
            'Marker',cfg.marker_shapes{j},...
            'MarkerEdgeColor','k','Color','w','LineWidth',1);
%             'MarkerEdgeColor','k','LineWidth',2,'MarkerSize',7,'Color','w');
        text(cfg.label_start ,ypos,par.value_label{j},cfg.format2{:});
        ypos = ypos - cfg.line_space; 
    end
    
    corner_pos(2) = corner_pos(2) - box_height;
end

function corner_pos = color_Style_legend(cfg,corner_pos, par)

%      par.value_label = par.value_label(~cellfun('isempty',par.value_label));
%      par.colors      = par.colors     (~cellfun('isempty',par.colors));

    if (~ isequal(par.label,''))
        box_height = cfg.line_space * (length(par.value_label)+1);
    else
        box_height = cfg.line_space *  length(par.value_label);
    end
    
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2;

    if (~ isequal(par.label,''))
        text(0.1,ypos, par.label,cfg.format1{:});
        ypos = ypos - cfg.line_space;
    end
    
    for j =1:length(par.value_label)
        line(cfg.marker_center + cfg.line_width *[-0.5 0.5],[ypos ypos],...
            'Color',par.colors{j},'LineStyle',cfg.line_styles{j},'LineWidth',1);

%             line(cfg.marker_center + cfg.line_width *[-0.5 0.5],[ypos ypos],...
%                 'LineStyle',cfg.line_styles{j},'LineWidth',1);
%             'MarkerEdgeColor','k','LineWidth',2,'MarkerSize',7,'Color','w');
        text(cfg.label_start ,ypos,par.value_label{j},cfg.format2{:});
        ypos = ypos - cfg.line_space; 
    end
    
    corner_pos(2) = corner_pos(2) - box_height;
end

function corner_pos = lineStyle_legend(cfg,corner_pos, par)

%      par.value_label = par.value_label(~cellfun('isempty',par.value_label));
%      par.colors      = par.colors     (~cellfun('isempty',par.colors));

    if (~ isequal(par.label,''))
        box_height = cfg.line_space * (length(par.value_label)+1);
    else
        box_height = cfg.line_space *  length(par.value_label);
    end
    
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2;

    if (~ isequal(par.label,''))
        text(0.1,ypos, par.label,cfg.format1{:});
        ypos = ypos - cfg.line_space;
    end
    
    for j =1:length(par.value_label)
        line(cfg.marker_center + cfg.line_width *[-0.5 0.5],[ypos ypos],...
            'Color','k','LineStyle',cfg.line_styles{j},'LineWidth',1);
        text(cfg.label_start ,ypos,par.value_label{j},cfg.format2{:});
        ypos = ypos - cfg.line_space; 
    end
    
    corner_pos(2) = corner_pos(2) - box_height;
end

function corner_pos = error_spread_legend(cfg,corner_pos)

    box_height = cfg.line_space * 2;
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2; 
    text(0.3 ,ypos,'RMSE',cfg.format2{:});
    line(cfg.marker_center + [-0.1 0.1],[ypos ypos],...
        'Color','k','LineWidth',1,'LineStyle','-');
    
    ypos = ypos - cfg.line_space; 
    text(0.3 ,ypos,'Spread',cfg.format2{:});
    line(cfg.marker_center + [-0.1 0.1],[ypos ypos],...
        'Color','k','LineWidth',1,'LineStyle',':');
    
    corner_pos(2) = corner_pos(2) - box_height;
end

function corner_pos = reference_run_legend(cfg,corner_pos,run_name)

    box_height = cfg.line_space;
    axes('pos',[corner_pos(1) corner_pos(2)-box_height cfg.legend_width box_height]);
    axis([0 1 0 box_height]);
    set(gca, 'YTickLabel', [],'YTick',[],'XTickLabel', [],'XTick',[]);

    ypos = box_height - cfg.line_space/2;
    text(cfg.marker_center + 0.5*cfg.line_width +0.05 ,ypos,run_name,cfg.format2{:});
    line(cfg.marker_center*[1 1] ,[ypos ypos],...
        'Marker',cfg.free_run_marker,cfg.line_opts{:},'LineWidth',1);
    line(cfg.marker_center + cfg.line_width *[-0.5 0.5],[ypos ypos],'Color','k');
    
    corner_pos(2) = corner_pos(2) - box_height;
end

% function corr_nature_Taver__obs_clean_Taver__plot(cfg)
% 
%     plot_name  = 'corr_nature__obs_clean_Taver__plot';
%            
%     % Import data
%     
%     for j=1:length(cfg.Tkind)
%         Tkind = cfg.Tkind{j};
%         stat = ['corr_nature_' Tkind '__obs_clean_Taver'];
%         netcdf_path = ['../run_sets/' cfg.run_set_names{1} '/stats/' stat '.nc'];
%         
%         corr_obs_nature {j} = get_scalar_netcdf(cfg,netcdf_path);
%         par1.value_label{j} = label(cfg.Tkind{j},'Tkind');
%     end
% 
%     
% %     strucdisp(corr_obs_nature);
% 
%     % Create line plots
% 
%     figure_hdl=figure();
%     set(figure_hdl,'Name',plot_name,'NumberTitle','off');
%     set(figure_hdl,'Position', [0 0 650 300]); clf;
% 
%     plot_width    = 0.3 ;  plot_height = 0.74 ;
%     plot_ypos     = 0.14;  plot_xpos   = 0.1 + [0  plot_width + 0.05];
% 
%     for l=1:2
%         axes('pos',[plot_xpos(l) plot_ypos plot_width plot_height]);
%         hold on;
%                 
%         for j=1:length(cfg.Tkind)
% 
%             line_color          = cfg.line_colors{j};
%             marker_shape        = cfg.marker_shapes{j};
%             par1.colors {j} = line_color;
%             par1.markers{j} = marker_shape;
% 
%             plot(corr_obs_nature{j}.dim.data,corr_obs_nature{j}.var(l).data,...
%             'LineStyle','-','Color',line_color,'Marker',marker_shape,...
%             'MarkerSize',8,'Linewidth',1,'MarkerEdgeColor',line_color);
%         
% %         for k=1:length(cfg.run_set_names)
% %             j1 = cfg.bunch_par1_value_pos(k)+1; 
% %             
% %             if isfield(cfg,'par_value_to_miss')
% %                 if isequal(cfg.bunch_par1_values{j1},...
% %                         cfg.par_value_to_miss.(cfg.bunch_par1_name{:})); 
% %                     continue
% %                 end
% %             end
% %             
% %             line_color   =   cfg.line_colors{j1};
% %             
% %             bunch_par1.value_label{j1} = ...
% %                 label(cfg.bunch_par1_values{j1},cfg.bunch_par1_name{:});
% %             bunch_par1.colors{j1} = line_color;
% %             
% %             if(cfg.bunch_dim == 1)
% %                 marker_shape = cfg.marker_shapes{j1};
% %                 bunch_par1.markers{j1} = marker_shape;
% %             else
% %                 j2 = cfg.bunch_par2_value_pos(k)+1;
% %                 marker_shape = cfg.marker_shapes{j2};
% % 
% %                 bunch_par2.value_label{j2} = ...
% %                     label(cfg.bunch_par2_values{j2},cfg.bunch_par2_name{:});
% %             end
%         end
%         
%         xlabel(corr_obs_nature{j}.dim.label);
%         if(l==1); ylabel('Pearson Correlation'); end
% 
%         axis tight; 
% %         ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [ylim_curr(1) 1.01]);
%         ylim([-0.41, 1.01]);
%         title(corr_obs_nature{j}.var(l).label);
%         refline(0,0);
%     end
% 
%     % Add Legends
%     
%     corner_pos = [(plot_xpos(2) + plot_width + 0.02) (plot_ypos + plot_height)];
%     par1.label = label('Tkind','par');
%     corner_pos =  color_legend(cfg,corner_pos, par1);
% %     if (cfg.bunch_dim == 2)
% %         bunch_par2.label = label(cfg.bunch_par2_name{:},'par');
% %         corner_pos = marker_legend(cfg,corner_pos, bunch_par2);
% %     end
% %     corner_pos = reference_run_legend(cfg,corner_pos,'Free Run');
% % %   legend(bunch_par1.value_label{1:end},'Location','EastOutside');
% % 
%     saveas(figure_hdl,[plot_name '.fig'],'fig');
%     export_fig(cfg.plot_formats{:},'-transparent',plot_name,figure_hdl);
% 
%     close(figure_hdl);
% end
% 

