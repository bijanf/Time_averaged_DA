function run_plot(cfg)
    fprintf('\n');
    print_function_message('opening')

    switch cfg.run_type{1}
        case 'nature'
            %     nature_run_plot(cfg);
            %      fprintf(2,' Nature plots \n');
            plot_speedy_state(cfg,'nature','timstd1','StdDev');
            plot_speedy_state(cfg,'nature','timmean','mean_v');
            
        case 'free'
            %       plot_scalar_vs_time(cfg,'free_prior_error','fldstd1_vertmean');
            plot_error_spread_vs_time(cfg,'free_prior');
      
            plot_speedy_state(cfg,'free_prior_error','timstd1','RMSErr');
            plot_speedy_state(cfg,'free_prior_Esprd','timmean','spread');
            plot_speedy_state(cfg,'free_prior_Emean','timstd1','StdDev');
            plot_speedy_state(cfg,'free_prior_Emean','timmean','mean_v');

        case 'assi'
            plot_station_set(cfg);
            %plot_error_spread_vs_time(cfg,'assi_prior');
            %plot_error_spread_vs_time(cfg,'assi_postr');
           % plot_error_spread_vs_time(cfg,'assi_prior',1);
           % plot_error_spread_vs_time(cfg,'assi_prior',2);
           % plot_error_spread_vs_time(cfg,'assi_postr',1);
           % plot_error_spread_vs_time(cfg,'assi_postr',2);
            
            for i=1:4
            plot_spreads_vs_time(cfg,'assi_postr',i,2,1)
            plot_spreads_vs_time(cfg,'assi_prior',i,2,1)
            plot_spreads_vs_time(cfg,'assi_postr',i,1,1)
            plot_spreads_vs_time(cfg,'assi_prior',i,1,1)
            
            plot_spreads_vs_time(cfg,'assi_postr',i,2,2)
            plot_spreads_vs_time(cfg,'assi_prior',i,2,2)
            plot_spreads_vs_time(cfg,'assi_postr',i,1,2)
            plot_spreads_vs_time(cfg,'assi_prior',i,1,2)
            
             %if  strcmp(cellstr(fileread('../config/obs_operator.cfg')),'norm_prod')==1 
             %            
             %  plot_errors_vs_time(cfg,'assi_prior',i,1)
             %  plot_errors_vs_time(cfg,'assi_postr',i,1)
             %  plot_errors_vs_time(cfg,'assi_prior',i,2)
             %  plot_errors_vs_time(cfg,'assi_postr',i,2)
            
             %end
            
            
            
            end
            %
            plot_speedy_state(cfg,'assi_prior_error','timstd1','RMSErr');
            plot_speedy_state(cfg,'assi_prior_Esprd','timmean','spread');
            plot_speedy_state(cfg,'assi_prior_Emean','timstd1','StdDev');
            plot_speedy_state(cfg,'assi_prior_Emean','timmean','mean_v');

            plot_speedy_state(cfg,'assi_postr_error','timstd1','RMSErr');
            plot_speedy_state(cfg,'assi_postr_Esprd','timmean','spread');
            plot_speedy_state(cfg,'assi_postr_Emean','timstd1','StdDev');
            plot_speedy_state(cfg,'assi_postr_Emean','timmean','mean_v');

            plot_speedy_state(cfg,'assi_prior_error','timstd1_DecreaseReffree_prior','ErrRed');
            plot_speedy_state(cfg,'assi_postr_error','timstd1_DecreaseReffree_prior','ErrRed');

            %if ~ isequal(cfg.obs_operator{1},'norm_T')
                %plot_speedy_state(cfg,'assi_prior_error','timstd1_IncreaseRefNormT_prior','ErrInc');
                %plot_speedy_state(cfg,'assi_postr_error','timstd1_IncreaseRefNormT_postr','ErrInc');
            %end
            
            %if      (~ isequal(cfg.obs_operator{1},'norm_T'  )) && ...
                    %(~ isequal(cfg.obs_operator{1},'norm_add'))
                %plot_speedy_state(cfg,'assi_prior_error','timstd1_IncreaseRefNormAdd_prior','ErrInc');
                %plot_speedy_state(cfg,'assi_postr_error','timstd1_IncreaseRefNormAdd_postr','ErrInc');
            %end

%                 if ~ isequal(cfg.obs_operator{1},'norm_min')
%                     %       plot_speedy_state(cfg,'assi_prior_error','timstd1_reducRefMin');
% %                     cfg.ErrRed.color_axis.t = [-40 40];
% %                     cfg.ErrRed.color_axis.q = [-40 40];
% %                     cfg.ErrRed.color_axis.u = [-40 40];
% %                     cfg.ErrRed.color_axis.v = [-40 40];
%                     plot_speedy_state(cfg,'assi_postr_error','timstd1_reducRefMin','ErrRed');
%                     plot_speedy_state(cfg,'assi_prior_error','timstd1_reducRefMin','ErrRed');
%                 end
%             end
            cfg.Tkind        = {'Tanom','Insta'};
%              plot_speedy_state(cfg,'assi_postr_Eskew','timmean','skewness');
    end
    
    print_function_message('closing')
end

function plot_speedy_state(cfg,state_name,time_stat,c_stat)
    print_function_message('opening')

    for it=1:2%length(cfg.Tkind)
        Tkind = cfg.Tkind{it};
        scalar   = [state_name '_grid_sigma_' Tkind '_' time_stat];
        disp([' ' scalar]);
        
        for l=1:length(cfg.var_name)
            vari.name = cfg.var_name{l};
            hor_stat = cfg.hor_stat.(vari.name);
            fprintf([' ' vari.name]);
            
%            for is=1:length(cfg.spatial_stats)
            file_path = ['../stats/' scalar '_' hor_stat '.nc'];
            vari.hor_stat = ...
                get_var_from_netcdf(cfg,file_path,vari.name);

            file_path = ['../stats/' scalar '_' 'zonmean' '.nc'];
            vari.zon_stat = ...
                get_var_from_netcdf(cfg,file_path,vari.name);

            vari.label      = cfg.var_label.(vari.name);
            vari.hor_label  = cfg.hor_label.(vari.name);
            vari.unit       = cfg.var_unit.(time_stat).(vari.name);

            vari.stat_label = cfg.(c_stat).label;
            vari.color_axis = cfg.(c_stat).color_axis.(vari.name);
            vari.color_map  = cfg.(c_stat).color_map_.(vari.name);
            
            plot_name      = [scalar '_' vari.name];
%             vari.hor_stat
%             vari.zon_stat
            lon_lat_and_zonal_plot(vari,plot_name);

            clear vari
        end
        fprintf('\n');
    end
    
    print_function_message('closing')
end

function plot_station_set(cfg)
    print_function_message('opening')

    file_path = '../raw_data/obs/station.tbl';
    plot_name = 'station_set';
    try
        obs_position = dlmread(file_path','',2,0);
    catch
        obs_position =[NaN NaN];
    end
    
    obs_position_lat = -90 + ((obs_position(:,2)-1)/48)*180;
    obs_position_lon = mod(((obs_position(:,1)-1)/96)*360 + 180,360)-180;

    
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 600 300]); clf;
    
%     title('Observation stations');
    
    m_proj('Equidistant cylindrical','long',[-180 180],'lat',[-90 90]);
    m_coast('color',[0 .6 0]);
    hold on
    m_plot(obs_position_lon,obs_position_lat,'o')
%     m_grid('box','fancy','tickdir','in');
    
%     set(gcf, 'PaperPosition', [0 0 24 12]);
%     print(gcf,'-dpsc2','-r0',[station_set '.eps'])
    set(gca,'XTick'     ,[    -pi,  -pi/2,  0 , pi/2])
    set(gca,'XTickLabel',{})
    set(gca,'YTick'     ,[-2*pi/6, -pi/6,   0, pi/6, 2*pi/6])
    set(gca,'YTickLabel',{})
    ylim([-pi pi]/2); xlim([-pi pi]); box on;

    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent','-eps','-pdf',plot_name,figure_hdl);
    close(figure_hdl);
    
    print_function_message('closing')
end


function plot_scalar_vs_time(cfg,state_name,operator)
    print_function_message('opening')

    line_color={'b','r','g','c','m'};
    for is=1:length(cfg.spatial_stats)            
        for l=1:length(cfg.var_name)
            var_name = cfg.var_name{l};
            disp ([' Plotting ' var_name]);

            clear var;
            for it=1:length(cfg.Tkind)
                scalar    = [state_name '_grid_sigma_' cfg.Tkind{it} '_' operator];
                file_path = ['../stats/' scalar '.nc'];

                var{it} = get_var_from_netcdf(cfg,file_path,var_name);
                var{it}.title  = var{it}.label;
                var{it}.xlabel = var{it}.time.label;
                var{it}.ylabel = operator;
                var{it}.legend = label(cfg.Tkind{it},'Tkind');
                var{it}.color  = line_color{it};
            end
            plot_name = [state_name '_grid_sigma_' operator '_' var_name];
            plot_stat_vs_time(var,plot_name);
        end
    end
    
    print_function_message('closing')
end

function plot_error_spread_vs_time(cfg,state_name,it)
    print_function_message('opening')

    line_color = {'b','r','g','c','m','y'};
    stat_id    = {'error','Esprd'};
    stat_label = {'Error','Spread'};
    operator   = {'fldstd1','fldmean'};

    for l=1:length(cfg.var_name)
        vari_name = cfg.var_name{l};
        hor_stat = cfg.hor_stat.(vari_name);
        fprintf([' ' vari_name]);
%         disp ([' Plotting ' vari.name]);

       clear vari; 
        n = 0;
        for is=1:length(stat_id)
            for it=1:length(cfg.Tkind)
                n = n + 1;
                 
                %length(cfg.Tkind)
                scalar    = [state_name '_' stat_id{is} '_grid_sigma_' ...
                    cfg.Tkind{it} '_' operator{is} '_' hor_stat];
                file_path = ['../stats/' scalar '.nc'];
				%vari{n}        = get_var_from_netcdf(cfg,file_path,vari_name);
				vari{n}        = get_var_from_netcdf(cfg,file_path,vari_name);
                %vari{n}.title  = vari{it}.label;
                vari{n}.title  = vari{n}.label;
               
                vari{n}.xlabel = vari{n}.time.label;
                vari{n}.ylabel = operator{is};
                vari{n}.legend = [label(cfg.Tkind{it},'Tkind') '_' stat_label{is}] ;
                vari{n}.ylim   = cfg.lim.RMSE.(vari_name);
%                vari{n}.legend = label(cfg.Tkind{it},'Tkind');
                vari{n}.color  = line_color{n};
            end
            
            plot_name = [state_name '_grid_sigma_' 'error-spread' '_' vari_name];
            plot_stat_vs_time(vari,plot_name);
        end
    end
    fprintf('\n');
    
    print_function_message('closing')
end

%function plot_error_spread_vs_time(cfg,state_name)
    %print_function_message('opening')

    %line_color = {'b','r','g','c','m','y'};
    %stat_id    = {'error','Esprd'};
    %stat_label = {'Error','Spread'};
    %operator   = {'fldstd1','fldmean'};

    %for l=1:length(cfg.var_name)
        %vari_name = cfg.var_name{l};
        %hor_stat = cfg.hor_stat.(vari_name);
        %fprintf([' ' vari_name]);
%%         disp ([' Plotting ' vari.name]);

        %clear vari; 
        %n = 0;
        %for is=1:length(stat_id)
            %for it=1:length(cfg.Tkind)
                %n = n + 1;

                %scalar    = [state_name '_' stat_id{is} '_grid_sigma_' ...
                    %cfg.Tkind{it} '_' operator{is} '_' hor_stat];
                %file_path = ['../stats/' scalar '.nc'];

                %vari{n}        = get_var_from_netcdf(cfg,file_path,vari_name);
                %vari{n}.title  = vari{it}.label;
                %vari{n}.xlabel = vari{it}.time.label;
                %vari{n}.ylabel = operator{is};
                %vari{n}.legend = [label(cfg.Tkind{it},'Tkind') '_' stat_label{is}] ;
                %vari{n}.ylim   = cfg.lim.RMSE.(vari_name);
%%                vari{n}.legend = label(cfg.Tkind{it},'Tkind');
                %vari{n}.color  = line_color{n};
            %end
            
            %plot_name = [state_name '_grid_sigma_' 'error-spread' '_' vari_name];
            %plot_stat_vs_time(vari,plot_name);
        %end
    %end
    %fprintf('\n');
    
    %print_function_message('closing')
%end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%             FREE vs DA       GLOBAL     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function plot_spreads_vs_time(cfg,state_name,l,is,it)
        %% l is the var_name : 
        % 1---t
        % 2---q
        % 3---u
        % 4---v
        %% is is the id 
        %1---Error
        %2---Spread
        %% it is the T kind
        % 1--- Time Averaged
        % 2--- Instantanous
    print_function_message('opening')
    
  


    line_color = {'b','r','g','c','m','y'};
    stat_id    = {'error','Esprd'};
    stat_label = {'Error','Spread'};
    operator   = {'fldstd1','fldmean'};

%l=1
        vari_name = cfg.var_name{l};
        hor_stat = cfg.hor_stat.(vari_name);
        fprintf([' ' vari_name]);
        clear vari; 
        n = 0;
%is=2
%it=1
                n = n + 1;
                 
                 state_name1='free_prior';
                 
                scalar    = [state_name1 '_' stat_id{is} '_grid_sigma_' ...
                    cfg.Tkind{it} '_' operator{is} '_' hor_stat];
                 
          %%%%%%%%%%%%%%%%%%%%% PLOT FREE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
                 
                  
                file_path = [ '../',cfg.rel_free_run_dir{1} '/stats/' scalar '.nc'];

				file_path
				vari{1}        = get_var_from_netcdf(cfg,file_path,vari_name);
			
				
                %vari{1}.title  = vari{1}.label;
                
                vari{1}.xlabel = vari{1}.time.label;
                vari{1}.ylabel = operator{is};
                vari{1}.legend = [cfg.Tkind{it} '-' stat_label{is} '-Free'] ;
                vari{1}.ylim   = cfg.lim.RMSE.(vari_name);
                vari{1}.color = [0 77 204]./255;
				%vari{1}.title  = [state_name1 '-' stat_label{is} '-'  cfg.Tkind{it} '-' vari_name]; 
                vari{1}.title  = vari{n}.label;
            plot_name = [state_name '_' stat_label{is} '_'  cfg.Tkind{it} '_' vari_name];
           
                
           
            
           %%%%%%%%%%%%%%%%%% PLOT ASSI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
           scalar    = [state_name '_' stat_id{is} '_grid_sigma_' ...
                    cfg.Tkind{it} '_' operator{is} '_' hor_stat];        
            file_path = ['../stats/' scalar '.nc'];
             
                vari{2}        = get_var_from_netcdf(cfg,file_path,vari_name);
                file_path
                %vari{2} .title  = vari{2} .label;
                vari{2}.xlabel = vari{2} .time.label;
                vari{2}.ylabel = operator{is};
                vari{2}.legend = [cfg.Tkind{it} '-' stat_label{is} '-DA' ] ;
                vari{2}.ylim   = cfg.lim.RMSE.(vari_name);
                vari{2}.color =[0 204 0]./255;
                %vari{2}.title  = [state_name '-' stat_label{is} '-'  cfg.Tkind{it} '-' vari_name]; 
                vari{2}.title  = vari{n}.label;
                 length(squeeze(vari{1}.data))
                 length(squeeze(vari{2}.data))
            plot_stat_vs_time(vari,plot_name); 
      
    fprintf('\n');
    
    print_function_message('closing')
end





%%%%%%%%%%%%%%%%%%%%%%%%%%%            PROD vs MIN   GLOBAL      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function plot_errors_vs_time(cfg,state_name,l,it)
        %% l is the var_name : 
        % 1---t
        % 2---q
        % 3---u
        % 4---v
        %% is is the id 
        %1---Error
        %2---Spread
        %% it is the T kind
        % 1--- Time Averaged
        % 2--- Instantanous
    print_function_message('opening')
    
  


    line_color = {'b','r','g','c','m','y'};
    stat_id    = {'error','Esprd'};
    stat_label = {'Error','Spread'};
    operator   = {'fldstd1','fldmean'};

%l=1
        vari_name = cfg.var_name{l};
        hor_stat = cfg.hor_stat.(vari_name);
        fprintf([' ' vari_name]);
        clear vari; 
        n = 0;
is=1
%it=1
                n = n + 1;
                scalar    = [state_name '_' stat_id{is} '_grid_sigma_' ...
                    cfg.Tkind{it} '_' operator{is} '_' hor_stat];        
            file_path = ['../../assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_min/stats/' scalar '.nc'];
            
                 
          %%%%%%%%%%%%%%%%%%%%% PLOT MIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
                 
                  
                

				file_path
				vari{1}        = get_var_from_netcdf(cfg,file_path,vari_name);
			
				
                %vari{1}.title  = vari{1}.label;
                
                vari{1}.xlabel = vari{1}.time.label;
                vari{1}.ylabel = operator{is};
                vari{1}.legend = [cfg.Tkind{it} '-' stat_label{is} '-Min'] ;
                vari{1}.ylim   = cfg.lim.RMSE.(vari_name);
                vari{1}.color = [0 77 204]./255;
				%vari{1}.title  = [state_name '-' stat_label{is} '-'  cfg.Tkind{it} '-' vari_name]; 
                vari{1}.title  = vari{n}.label;
            plot_name = [state_name '_' stat_label{is} '_'  cfg.Tkind{it} '-Prod-vs-Min_' vari_name];
           
                
           
            
           %%%%%%%%%%%%%%%%%% PLOT PROD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
           scalar    = [state_name '_' stat_id{is} '_grid_sigma_' ...
                    cfg.Tkind{it} '_' operator{is} '_' hor_stat];        
            file_path = ['../stats/' scalar '.nc'];
             
                vari{2}        = get_var_from_netcdf(cfg,file_path,vari_name);
                file_path
                %vari{2} .title  = vari{2} .label;
                vari{2}.xlabel = vari{2} .time.label;
                vari{2}.ylabel = operator{is};
                vari{2}.legend = [cfg.Tkind{it} '-' stat_label{is} '-Prod' ] ;
                vari{2}.ylim   = cfg.lim.RMSE.(vari_name);
                vari{2}.color =[0 204 0]./255;
                %vari{2}.title  = [state_name '-' stat_label{is} '-'  cfg.Tkind{it} '-' vari_name]; 
                vari{2}.title  = vari{n}.label;
                 length(squeeze(vari{1}.data))
                 length(squeeze(vari{2}.data))
            plot_stat_vs_time(vari,plot_name); 
      
    fprintf('\n');
    
    print_function_message('closing')
end



%    strucdisp(nature_Zonmean);
%     if(isequal(cfg.run_mode{1},'free'))
%         nature_dir='../raw_data';
%     else
%         nature_dir=['../' cfg.rel_run_free_dir{1} '/raw_data'];
%     end
%     
%     fprintf(2,' Observation plots \n');
%     if(isequal(cfg.run_mode{1},'assi'))
% %       observation_plots(cfg,nature_Insta);
%       model_obs_plots(cfg,nature_Insta);
%     end



%  function nature_run_plot(cfg)
%      for is=1:length(cfg.stats)
%          for it=1:length(cfg.Tkind)
%              scalar   = ['nature_grid_sigma_' cfg.Tkind{it} '_' cfg.stats{is}]
%              file_path= ['../stats/' scalar '.nc'];
%              
%              for l=1:length(cfg.var_name)
%                  vari.name = cfg.var_name{l};
%                  disp ([' Plotting ' vari.name]);
%  
%                  var = get_var_from_netcdf(cfg,file_path,vari.name);
%                  vari.color_axis = cfg.color_axis.(vari.name).Tstd
%                  plot_name = [scalar '_' vari.name];
%                  switch cfg.stats{is}
%                      case 'zonmean_Tstdd'
%  %                        zonal_plot_(var,plot_name);
%                          zonal_plot2(var,plot_name);
%                      case 'vertmean_Tstdd'
%                          lon_lat_plot(var,plot_name);
%                  end
%              end
%          end
%      end
%  end    

    



