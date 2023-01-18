function observation_plots(cfg,state)
    fprintf(2,' Observation plots \n');
   obs_time_series      (cfg,state)
%   time_series_2    (cfg,state)
%   pseudo_atractors (cfg,state)
%   correlation      (cfg,state)
%   spectral_analysis(cfg,state)
end


function obs_time_series(cfg,state)
    line_span=200:3000;
    for j=1:2
        state_var{j} = squeeze(state.value.var(j).data(:,1,1,:));
        state_min{j} = squeeze(state.min.var(j).data(:,1,1,:));
        state_max{j} = squeeze(state.max.var(j).data(:,1,1,:));
        resp     {j} = response_function(state_var{1,j}(:,line_span),...
            state_min{1,j}(1,:),state_max{1,j}(1,:));
    end
    time=state.value.dim.data(line_span);
    
    clean_obs = squeeze(state.obs.var.data(:,1,1,line_span));
    time_obs  = state.obs.dim.data(line_span);
    
    hdl = figure();
    plot_title = [cfg.model_name{1} ' response functions'];
    
    hold on;
    for j=1:2
        subplot(3,1,j);
        hold on;
        plot(time, resp{1}(j,:),'r');
        plot(time, resp{2}(j,:),'b');
        plot(time, clean_obs(j,:),'g');
        
        if(j==1); title(plot_title,'FontWeight','bold'); end
        if(j==3); xlabel(state.value.dim.name); end
        axis tight;
    end
    
    plot_name = 'nature_plot_6';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% RESPONSE FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [gX] = response_function(X,X_down,X_up)
    gX = NaN(size(X));
    gX(X_down >  X           ) = 0;
    gX(X_down <= X & X <=X_up) = (X(X_down<=X & X<=X_up)-X_down)/(X_up-X_down);
    gX(              X > X_up) = 1;
end

% function time_series_2(cfg,state)
%     for j=1:2
%         state_var{j}= squeeze(state.var(j).data(:,1,1,:));
%     end
% 
%     hdl = figure();
%     plot_title = [cfg.model_name{1} ' time_series'];
%     
%     hold on;
%     for j=1:3
%         subplot(3,1,j);
%         hold on;
%         plot(state.dim.data, squeeze(state.var(1).data(j,1,1,:)),'b');
%         plot(state.dim.data, squeeze(state.var(2).data(j,1,1,:)),'r');
%         legend('x','y','z','Location','SouthEast');
%         xlabel(state.dim.name); 
%         ylabel(['Component ' num2str(j)]);
%         if(j==1); title(plot_title,'FontWeight','bold'); end
%         axis tight;
%     end
% 
%     plot_name = 'nature_plot_1';
%     saveas(hdl,[plot_name '.fig'],'fig');
%     export_fig('-transparent',[plot_name '.pdf'],hdl);
%     close(hdl);
% end
% 
% function pseudo_atractors(cfg,state)
%     for j=1:2
%         state_var{j}= squeeze(state.var(j).data(:,1,1,:));
%     end
% 
%     hdl=figure();
%     plot_name   = [cfg.model_name{1} ' atractor'];
%     set(hdl,'Position', [0 0 600 350]); 
% 
%     plot_width    = 0.4;   plot_height = 0.74 ;
%     plot_ypos     = 0.14;   plot_xpos   = [0.05  0.56];
% 
%     for j=1:2
%         axes('pos',[plot_xpos(j) plot_ypos plot_width plot_height]);
%         hold on;
%         plot3(state_var{j}(1,:),state_var{j}(2,:),state_var{j}(3,:),'b');
%         view(-148,14);
% %         xlabel('x'); ylabel('y'); zlabel('z');
%         title(['Component ' num2str(j)]);
%         axis tight; grid on;
%     end
%     mtit(plot_name,'FontWeight','bold');
% 
%     % save    
%     plot_name = 'nature_plot_2';
%     saveas(hdl,[plot_name '.fig'],'fig');
%     export_fig('-transparent',[plot_name '.pdf'],hdl);
%     close(hdl);
% end
% 
% function correlation(cfg,state)
%     for j=1:2
%         state_var{j}= squeeze(state.var(j).data(:,1,1,:));
%     end
% 
%     for j=1:3
%         slow_fast_corr(j) = corr(state_var{1}(j,:)',state_var{2}(j,:)');
%     end        
% 
%     hdl=figure();
%     plot_title   = [cfg.model_name{1} ' Slow-fast variable correlation'];
%     set(hdl,'Position', [0 0 600 300]); 
% 
%     plot(slow_fast_corr,'-o','Color','r');
%     refline([0 0]);
% 
%     xlabel('Variable position');
%     ylabel('Pearson Correlation')
%     ylim([-1 1]);
% %     xlim([0 0.01]); %xlim([0 0.001]);
%     title(plot_title,'FontWeight','bold');
%     plot_name = 'nature_plot_3';
%     saveas(hdl,[plot_name '.fig'],'fig');
%     export_fig('-transparent',[plot_name '.pdf'],hdl);
%     close(hdl);
% end
% 
% function spectral_analysis(cfg,state)
%     for j=1:2
%         state_var{j}= squeeze(state.var(j).data(:,1,1,:));
%     end
% 
%     hdl=figure();
%     set(hdl,'Position', [0 0 600 400]); 
%     hold on;
%     plot_title  = [cfg.model_name{1} ' periodogram'];
%     
%     color_id={'r','b'};
%     for j=1:2
% %         [pxx,f] = pwelch(state_var{j}(1,:),500,300,500,cfg.cycle_length);
% %         [pxx,f] = pwelch(state_var{j}(1,:));
%         [pxx,f] = pwelch(state_var{j}(1,:),[],[],[],cfg.cycle_length);
% %         [pxx,f] = periodogram(state_var{j}(1,:),[],[],cfg.dt);
%         %     plot(f,pxx,'b');  ylabel('power');
%         plot(f,10*log10(pxx),color_id{j});  ylabel('dB');
%     end
%         
%     xlabel('frequency');
%     title(plot_title,'FontWeight','bold');
%     legend('Component 1','Component 2','Location','NorthEast');
% 
%     % ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
%     % xlim([0 0.01]); %xlim([0 0.001]);
%     axis tight;
%     ylim([-40 60]);
% 
%     plot_name = 'nature_plot_4';
%     saveas(hdl,[plot_name '.fig'],'fig');
%     export_fig('-transparent',[plot_name '.pdf'],hdl);
%     
%     close(hdl);
% end
