function model_obs_plots(cfg,state)
    fprintf(2,' Observation plots \n');
   obs_time_series      (cfg,state)
end

function obs_time_series(cfg,state)
    line_span=200:3000;
    for l=1:2
        for m=1:length(state.value.var(l).data(:,1,1,1))
            state_var     {l}= squeeze(state.value.var(l).data(m,1,1,:));
            threshold_down{l}= squeeze(state.threshold_down.var(l).data(m,1,1,:));
            threshold_up  {l}= squeeze(state.threshold_up.var(l).data(m,1,1,:));
            resp{l}(m,:) = ramp(state_var{l},threshold_down{l},threshold_up{l});
        end
    end
    
    clean_obs = squeeze(state.obs.var.data(:,1,1,:));
    time=state.value.dim.data(:);

    hdl = figure();
    plot_title = [cfg.model_name{1} ' response functions'];
    
    hold on;
    for j=1:3
        subplot(3,1,j);
        hold on;
        plot(time, resp{1}(j*2,:),'r');
        plot(time, resp{2}(j*2,:),'b');
        plot(time, clean_obs(j,:),'g');
        
        if(j==1); title(plot_title,'FontWeight','bold'); end
        if(j==3); xlabel(state.value.dim.name); end
        axis tight;
        legend('Resp1','Resp2','Resp_Add');
    end
    
    plot_name = 'nature_plot_6';
    saveas(hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],hdl);
    close(hdl);
end