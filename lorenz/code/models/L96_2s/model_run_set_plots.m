function model_run_set_plots(cfg)
    print_function_message('opening')

    cfg.marker_shapes = {'s','o','^','v','x'};
    cfg.line_colors   = {'m','r','b','g','c','m'};

    s_length = length(cfg.nature.value.dim.data(:));
    p_length = 600;
%     cfg.span=1:s_length;
    cfg.span = 500 + (1:p_length);
%     cfg.span=101:s_length-300;
%     cfg.span=1:s_length-200;
%     cfg.span=201:s_length;

    cfg.time=cfg.nature.value.dim.data(cfg.span)*0.01;
    for j=1:2
        cfg.state_var{j} = ...
            squeeze(cfg.nature.value.var(j).data(:,1,1,cfg.span));
    end

    for obs_ind = 1:10
        obs_time_series  (cfg,obs_ind);
    end
    time_series      (cfg);
    time_series_2    (cfg);
%     correlation      (cfg);
%     spectral_analysis(cfg);    

    print_function_message('closing')
end

function obs_time_series(cfg,obs_ind)
    print_function_message('opening')

    plot_height = 0.25;
    plot_width = 0.7;
    legend_width = 0.5;
    
%     obs_ind = 1;
%    sta_ind = obs_ind *2;
    sta_ind = obs_ind;

    for l=1:2
%         for m=2:2%length(state.value.var(l).data(:,1,1,1))
        nature_data   {l}= squeeze(cfg.nature.value.var(l).data(sta_ind,1,1,cfg.span));
        threshold_down{l}= squeeze(cfg.assirun{l}.threshold_down.var(l).data(sta_ind,1,1,:));
        threshold_up  {l}= squeeze(cfg.assirun{l}.threshold_up.var(l).data(sta_ind,1,1,:));
        resp{l}(:,:) = ramp(nature_data{l},threshold_down{l},threshold_up{l});
%         end
    end
    for j=1:length(cfg.assirun)
        obs_data{j} = squeeze(cfg.assirun{j}.obs.var.data(obs_ind,1,1,cfg.span));
    end

    %%
   
    plot_name = ['obs_plot__ind_' num2str(obs_ind)];
    hdl = figure('Position',[0 0 600 500]); 

%%
    plot_title = 'Response window';
    
    hdl1 = subplot(3,1,1);
%    hdl1=subplot('Position',[0.1 0.7 plot_width plot_height]);
    hold on;

    x = [        min(cfg.time);       min(cfg.time);       max(cfg.time);         max(cfg.time)];
    y = [threshold_down{1}; threshold_up{1}; threshold_up{1}; threshold_down{1}];
    tcolor = [.7 .7 .7]; rect_H = patch(x,y,tcolor);
    h= findobj(gca, 'Type', 'patch'); 
    set(h,'FaceColor', [.85 .85 .85], 'EdgeColor', [.85 .85 .85]);

    plot(cfg.time, nature_data{1},'r');
    
    axis tight; yl=ylim; ylim((yl(2)+yl(1))/2+[-0.6 0.6]*(yl(2)-yl(1)));
    ylabel(['T_{' num2str(sta_ind) '}'],'rot',0);
    lh= legend(sprintf('Response   \nWindow'),sprintf('Variable'),...
        'Location','SouthEastOutside');
%      htitle = get(lh,'Title'); 
%      set(htitle,'String','Fast Component','fontWeight','bold');
%      legend('boxoff');
%      title('(a)')

%%
    plot_title = 'Growth Response';
        
     hdl1 = subplot(3,1,2);
%    axes('Position',[0.1 0.4 plot_width plot_height]);
    hold on;

    plot(cfg.time, resp{1},'r');
    plot(cfg.time, resp{2},'b');

    axis tight; ylim([-0.05 1.05]);
    ylabel(plot_title); 
    
    lh = legend('Fast Comp.','Slow Comp.','Location','SouthEastOutside');
%    legend('boxoff');
%     hleg = legend('Comp. 1','Comp. 2','Location','EastOutside');
%     htitle = get(lh,'Title'); 
%     set(htitle,'String',plot_title,'fontWeight','bold');
%     set(gca, 'XTickLabel', []);
%     title('(b)')

%%
    plot_title = 'Growth Rate';
    
     hdl1 = subplot(3,1,3);
%    axes('Position',[0.1 0.1 plot_width plot_height]);
    hold on;
    
    for j=2:length(cfg.assirun)
        plot(cfg.time, obs_data{j}(:), cfg.line_colors{j});
        line_labels{j}=cfg.assirun{j}.label;
    end
    axis tight
    ylim([-0.05 1.05]);
%     yl = ylim; ylim([-0.05 yl(2)+0.05]);
    xlabel('time'); ylabel(plot_title);
    
    lh = legend(line_labels{:},'Location','SouthEastOutside');
    htitle = get(lh,'Title'); 
    set(htitle,'String','T-norm','fontWeight','bold');
%     set(htitle,'String',plot_title,'fontWeight','bold');
%     title('(c)')
    
%%    
    % save figure
    
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-pdf','-eps','-transparent',plot_name,hdl);
    close(hdl);
    
    print_function_message('closing')
end

function time_series(cfg)
    print_function_message('opening')

%     hdl = figure();
%     plot_title = 'time series';
    
    for j=1:5
hdl = figure();
    plot_title = 'time series';        
        subplot(3,1,1);
        hold on;
%         plotyy(cfg.time, cfg.state_var{1}(j,:),cfg.time, cfg.state_var{2}(j,:));
        plot(cfg.time, cfg.state_var{1}(j,:),'r');
        plot(cfg.time, cfg.state_var{2}(j,:),'b');
        axis tight
%         legend('x1','x2','x3','Location','SouthEast');
%         xlabel(cfg.nature.value.dim.name); 
%         ylabel(['Component ' num2str(j)]);
%         if(j==1); title(plot_title,'FontWeight','bold'); end
%         axis tight;
    plot_name = ['T-M_times_series_' num2str(j)];
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('.pdf','-transparent',plot_name,hdl);
    close(hdl);
        
    end
    for j=1:5
hdl = figure();
    plot_title = 'time series';        
        subplot(3,1,1);
        hold on;
        [ax,h1,h2] = plotyy(cfg.time, cfg.state_var{1}(j,:),cfg.time, cfg.state_var{2}(j,:));
        set(ax(1),'YLim',[-7 12])
        set(ax(2),'YLim',[-1 4])

%         legend('x1','x2','x3','Location','SouthEast');
%         xlabel(cfg.nature.value.dim.name); 
%         ylabel(['Component ' num2str(j)]);
%         if(j==1); title(plot_title,'FontWeight','bold'); end
%         axis tight;
    plot_name = ['T-M_times_series_' num2str(j+5)];
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('.pdf','-transparent',plot_name,hdl);
    close(hdl);
        
    end
%     for j=1:2
%         subplot(2,1,j);
%         hold on;
%         plot(cfg.time, cfg.state_var{j}(1,:),'b');
%         plot(cfg.time, cfg.state_var{j}(2,:),'r');
%         plot(cfg.time, cfg.state_var{j}(3,:),'g');
%         legend('x1','x2','x3','Location','SouthEast');
%         xlabel(cfg.nature.value.dim.name); 
%         ylabel(['Component ' num2str(j)]);
%         if(j==1); title(plot_title,'FontWeight','bold'); end
%         axis tight;
%     end

%     plot_name = 'nature_plot_1';
%     saveas(hdl,[plot_name '.fig'],'fig');
%     export_fig('.pdf','-transparent',plot_name,hdl);
%     close(hdl);
    print_function_message('closing')
end

function time_series_2(cfg,state)
    print_function_message('opening')

    hdl = figure();
    cfg
    plot_title = [cfg.model{1} ' time_series'];
    
    hold on;
    for j=1:3
        subplot(3,1,j);
        hold on;
        plot(state.dim.data, squeeze(state.var(1).data(j,1,1,:)),'b');
        plot(state.dim.data, squeeze(state.var(2).data(j,1,1,:)),'r');
        legend(['x' num2str(j)],['y' num2str(j)],'Location','SouthEast');
        xlabel(state.dim.name); 
        ylabel(['Component ' num2str(j)]);
        if(j==1); title(plot_title,'FontWeight','bold'); end
        axis tight;
    end

    plot_name = 'nature_plot_5';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
    
    print_function_message('closing')
end

function correlation(cfg,state)
    print_function_message('opening')
    
    l=0;
    for j=1:4
        for k=1:4
            l=l+1;
            slow_fast_corr(l) = ...
                corr(cfg.state_var{1}(j,:)',cfg.state_var{2}(k,:)');
            ticks{l}=[num2str(j) '.' num2str(k)];
        end
    end        

    hdl=figure();
    plot_title   = 'Slow-fast variable correlation';
    set(hdl,'Position', [0 0 600 300]); 

    bar(slow_fast_corr);
    refline([0 0]);
    %     set(gca,'XTick',[0:0.5:4])
    set(gca,'XTick'     ,1:l)
    set(gca,'XTickLabel',ticks)
   

    xlabel('Fast-slow variable indices');
    ylabel('Pearson Correlation')
    ylim([-1 1]); xlim([0 l]+0.5);
%     xlim([0 0.01]); %xlim([0 0.001]);
    title(plot_title,'FontWeight','bold');
    plot_name = 'nature_plot_3';
    saveas(hdl,plot_name,'fig');
    export_fig('-pdf','-transparent',plot_name,hdl);
    close(hdl);

    print_function_message('closing')
end

function spectral_analysis(cfg,state)
    print_function_message('opening')

    hdl=figure();
    set(hdl,'Position', [0 0 600 400]); 
    hold on;
    plot_title  = 'Periodogram';
    
    color_id={'r','b'};
    for j=1:2
%         [pxx,f] = pwelch(cfg.state_var{j}(1,:),500,300,500,cfg.cycle_length);
%         [pxx,f] = pwelch(cfg.state_var{j}(1,:));
        [pxx,f] = pwelch(cfg.state_var{j}(1,:),[],[],[],cfg.cycle_length);
%         [pxx,f] = periodogram(cfg.state_var{j}(1,:),[],[],cfg.dt);
        %     plot(f,pxx,'b');  ylabel('power');
        plot(f,10*log10(pxx),color_id{j});  ylabel('dB');
    end
        
    xlabel('frequency');
    title(plot_title,'FontWeight','bold');
    legend('Component 1','Component 2','Location','NorthEast');

    % ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
    % xlim([0 0.01]); %xlim([0 0.001]);
    axis tight;
    ylim([-40 60]);

    plot_name = 'nature_plot_4';
    saveas(hdl,plot_name,'fig');
    export_fig('-pdf','-transparent',plot_name,hdl);
    
    close(hdl);

    print_function_message('closing')
end
