function model_obs_plots(cfg,state)
    fprintf(2,' Observation plots \n');
   obs_time_series      (cfg,state)
end


function obs_time_series(cfg,state)
    line_span=200:1000;
    for j=1:2
        state_var{j} = squeeze(state.value.var(j).data(:,1,1,:));
        state_min{j} = squeeze(  state.min.var(j).data(:,1,1,:));
        state_max{j} = squeeze(  state.max.var(j).data(:,1,1,:));
        state_mean{j} = squeeze(  state.mean.var(j).data(:,1,1,:));
        state_stdd{j} = squeeze(  state.stdd.var(j).data(:,1,1,:));
        
        for k=1:cfg.nc
            
%             resp  {j,k} = response_function(state_var{1,j}(k,line_span),...
%                 state_min{1,j}(k,1),state_max{1,j}(k,1));
            resp  {j,k} = normalize(state_var{1,j}(k,line_span),...
                state_mean{1,j}(k,1),state_stdd{1,j}(k,1));
            
        end
    end
    
    time=state.value.dim.data(line_span) * cfg.dt ;
    
    clean_obs = squeeze(state.obs.var.data(:,1,1,line_span));
    time_obs  = state.obs.dim.data(line_span);
    
    hdl = figure();
    plot_title = [cfg.model_name{1} ' observation time series'];
    
    hold on;
    for j=1:3
        subplot(3,1,j);
        
        hold on;
        plot(time, resp{1,j*2}(:),'r');
        plot(time, resp{2,j*2}(:),'b');
        plot(time, clean_obs(j,:),'g');

        %force = cfg.F_mean + sin(2*pi*time / cfg.F_tau);
        %force = 1 + sin(2*pi*time / cfg.F_tau);
        %plot(time_obs, force,'k');
        
        if(j==1); title(plot_title,'FontWeight','bold'); end
        if(j==3); xlabel(state.value.dim.name); end
        axis tight;
        leg_hdl = legend('R(x)','R(X)','Growth Rate');
        set(leg_hdl,'Location','EastOutside');
%         set(leg_hdl,'Interpreter','Latex');
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% NORMALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [gX] = normalize(X,X_mean,X_stdd)
    gX = (X - X_mean)/X_stdd;
end