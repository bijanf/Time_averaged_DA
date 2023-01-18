function model_nature_plots(cfg,state)
   time_series      (cfg,state)
   time_series_2    (cfg,state)
%   pseudo_atractors (cfg,state)
   correlation      (cfg,state)
   spectral_analysis(cfg,state)
end


function time_series(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl = figure();
    plot_title = [cfg.model_name{1} ' time_series 1'];
    
    hold on;
    for j=1:2
        subplot(2,1,j);
        hold on;
        
        time = state.dim.data * cfg.dt ;
        
        plot(time, squeeze(state.var(j).data(1,1,1,:)),'r');
        plot(time, squeeze(state.var(j).data(2,1,1,:)),'b');
        plot(time, squeeze(state.var(j).data(3,1,1,:)),'g');

        %force = cfg.F_mean + cfg.F_ampl * sin(2*pi*time / cfg.F_tau);
        force = sin(2*pi*time / cfg.F_tau);
        plot(time, force,'k');

        legend('x1','x2','x3','Location','SouthEast');
        xlabel(state.dim.name); 
        ylabel(['Component ' num2str(j)]);
        if(j==1); title(plot_title,'FontWeight','bold'); end
        axis tight;
    end

    plot_name = 'nature_plot_1';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end

function time_series_2(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl = figure();
    plot_title = [cfg.model_name{1} ' time_series 2'];
    
    hold on;
    for j=1:3
        subplot(3,1,j);
        hold on;

        time = state.dim.data * cfg.dt ;
        
        plot(time, squeeze(state.var(1).data(j,1,1,:)),'r');
        plot(time, squeeze(state.var(2).data(j,1,1,:)),'b');

        
        force = cfg.F_mean + cfg.F_ampl * sin(2*pi*time / cfg.F_tau);
        plot(time, force,'k');

        legend(['x' num2str(j)],['y' num2str(j)],'Forcing','Location','SouthEast');
        xlabel(state.dim.name); 
        ylabel(['Component ' num2str(j)]);
        if(j==1); title(plot_title,'FontWeight','bold'); end
        axis tight;
    end

    plot_name = 'nature_plot_5';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end

function pseudo_atractors(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl=figure();
    plot_name   = [cfg.model_name{1} ' atractor'];
    set(hdl,'Position', [0 0 600 350]); 

    plot_width    = 0.4;   plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = [0.05  0.56];

    for j=1:2
        axes('pos',[plot_xpos(j) plot_ypos plot_width plot_height]);
        hold on;
        plot3(state_var{j}(1,:),state_var{j}(2,:),state_var{j}(3,:),'b');
        view(-148,14);
%         xlabel('x'); ylabel('y'); zlabel('z');
        title(['Component ' num2str(j)]);
        axis tight; grid on;
    end
    mtit(plot_name,'FontWeight','bold');

    % save    
    plot_name = 'nature_plot_2';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end

function correlation(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    for j=1:3
        slow_fast_corr(j) = corr(state_var{1}(j,:)',state_var{2}(j,:)');
    end        

    hdl=figure();
    plot_title   = [cfg.model_name{1} ' Slow-fast variable correlation'];
    set(hdl,'Position', [0 0 600 300]); 

    plot(slow_fast_corr,'-o','Color','r');
    refline([0 0]);

    xlabel('Variable position');
    ylabel('Pearson Correlation')
    ylim([-1 1]);
%     xlim([0 0.01]); %xlim([0 0.001]);
    title(plot_title,'FontWeight','bold');
    plot_name = 'nature_plot_3';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end

function spectral_analysis(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl=figure();
    set(hdl,'Position', [0 0 600 400]); 
    hold on;
    plot_title  = [cfg.model_name{1} ' periodogram'];
    
    color_id={'r','b'};
    for j=1:2
%         [pxx,f] = pwelch(state_var{j}(1,:),500,300,500,cfg.cycle_length);
%         [pxx,f] = pwelch(state_var{j}(1,:));
        [pxx,f] = pwelch(state_var{j}(1,:),[],[],[],cfg.cycle_length);
%         [pxx,f] = periodogram(state_var{j}(1,:),[],[],cfg.dt);
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
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    
    close(hdl);
end
