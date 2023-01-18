function model_nature_plots(cfg,state)
    print_function_message('opening')

    time_series      (cfg,state)
    pseudo_atractors (cfg,state)
    correlation      (cfg,state)
    spectral_analysis(cfg,state)

    print_function_message('closing')
end


function time_series(cfg,state)
    print_function_message('opening')

    step_span = 1:1:5000;
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl = figure('Position',[0,0,400,400]);
    plot_title = [cfg.model_name{1} ' time_series'];
    
    hold on;
    for j=1:2
        subplot(2,1,j);
        hold on;
        plot(state.dim.data(step_span), squeeze(state.var(j).data(1,1,1,step_span)),'b');
        plot(state.dim.data(step_span), squeeze(state.var(j).data(2,1,1,step_span)),'r');
        plot(state.dim.data(step_span), squeeze(state.var(j).data(3,1,1,step_span)),'g');
        if(j==1); legend('x_1','x_2','x_3','Location','SouthEast'); end
        if(j==2); legend('x_4','x_5','x_6','Location','SouthEast'); end

        if(j==1); set(gca,'XTickLabel',''); end
        if(j==2); xlabel(state.dim.name); end
        ylabel(['Component ' num2str(j)]);
%          if(j==1); title(plot_title,'FontWeight','bold'); end
        axis tight;
    end

    plot_name = 'nature_plot_1';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-pdf','-eps','-transparent',plot_name,hdl);
    close(hdl);

    print_function_message('closing')
end

function pseudo_atractors(cfg,state)
    print_function_message('opening')

    step_span = 1:1:50000;
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl=figure('Position',[0,0,1200,600]);
    plot_name   = [cfg.model_name{1} ' atractor'];
    set(hdl,'Position', [0 0 600 350]); 

    plot_width    = 0.4;    plot_height = 0.74 ;
    plot_ypos     = 0.14;   plot_xpos   = [0.05  0.56];

    for j=1:2
        axes('pos',[plot_xpos(j) plot_ypos plot_width plot_height]);
        hold on;
        plot3(state_var{j}(1,step_span),state_var{j}(2,step_span),state_var{j}(3,step_span),'b');
        view(-148,14);
        if(j==1); xlabel('x_1'); ylabel('x_2'); zlabel('x_3'); end
        if(j==2); xlabel('x_4'); ylabel('x_5'); zlabel('x_6'); end
        title(['Component ' num2str(j)]);
        axis tight; grid on;
    end
%      mtit(plot_name,'FontWeight','bold');

    % save    
    plot_name = 'nature_plot_2';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent','-pdf','-eps',plot_name,hdl);
    close(hdl);

    print_function_message('closing')
end

function correlation(cfg,state)
    print_function_message('opening')
    
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    
    l=0;
    for j=1:3
        for k=1:3
%              if isequal(j,k); continue; end
            l=l+1;
            slow_fast_corr(l) = corr(state_var{1}(j,:)',state_var{2}(k,:)');
            ticks{l}=[num2str(j) '-' num2str(k+3)];
        end
    end        

    hdl=figure();
    plot_title   = [cfg.model_name{1} ' Slow-fast variable correlation'];
    set(hdl,'Position', [0 0 600 300]); 

    bar(slow_fast_corr);
    colormap jet
    refline([0 0]);
    %     set(gca,'XTick',[0:0.5:4])
    set(gca,'XTickLabel',ticks)

    

    xlabel('Variable indices');
    ylabel('Pearson Correlation')
    ylim([-1 1]);
%     xlim([0 0.01]); %xlim([0 0.001]);
%      title(plot_title,'FontWeight','bold');
    plot_name = 'nature_plot_3';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent','-pdf','-eps',plot_name,hdl);
    close(hdl);

    print_function_message('closing')
end

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
%     set(hdl,'Position', [0 0 450 300]); 
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
function spectral_analysis(cfg,state)
    for j=1:2
        state_var{j}= squeeze(state.var(j).data(:,1,1,:));
    end

    hdl=figure();
    set(hdl,'Position', [0 0 450 300]); 
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
%      title(plot_title,'FontWeight','bold');
    legend('Component 1','Component 2','Location','NorthEast');

    % ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [0 ylim_curr(2)]);
    %xlim([0 0.01]); %xlim([0 0.001]);
    axis tight;
%     ylim([-40 60]);

    xlim([0 0.01]);
    ylim_curr = get(gca,'ylim'); set(gca, 'ylim', [-10 ylim_curr(2)]);

    plot_name = 'nature_plot_4';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-pdf','-eps','-transparent',plot_name,hdl);
    
    close(hdl);
end
