function plot_hist_vs_time(var,plot_name,varargin)
    
    if not(iscell(var))
        var_tmp = var;
        clear var;
        var{1}  = var_tmp;
    end    
    
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 800 300]); clf;
    
    hold on;
    k=1;
    col{1}='red'
    col{2}='k'
    for it=1:length(var)
       % plot(var{it}.time.data, squeeze(var{it}.data),...
       %     '-','Color',var{it}.color,'MarkerFaceColor','none', 'linewidth',1,'MarkerSize',4);
       % [N,X] = hist(squeeze(var{it}.data),20);
	%	h= bar(X,N,0.5)
	%	set(h,'FaceColor',var{it}.color);
       h=histfit(squeeze(var{it}.data),20, 'kernel');  
       set(h(1),'FaceColor',var{it}.color);
      % set(h(1),'FaceColor','w');
       
       %set(h(1),'EdgeColor','w');
       set(h(2),'Color',col{it});
       %plot([min(var{it}.time.data) max(var{it}.time.data)],[mean(squeeze(var{it}.data)) mean(squeeze(var{it}.data))],'--','Color',var{it}.color, 'linewidth',3)
        legend_string{k} = [ 'Hist. of ', var{it}.legend ];  
       % legend_string{k} = '' 
        legend_string{k+1} = [ 'PDF of ', var{it}.legend ];    
       % k=k+2;
       k=k+2;
    end

    legend(legend_string{:},'Location','NorthEastOutside');
    
    %if isfield(var,'color_axis')
        %caxis(var.color_axis);
    %end

    axis tight
    %if isfield(var{it},'ylim'); ylim(var{it}.ylim); end
    if isfield(var{it},'xlim'); xlim(var{it}.xlim); end
    %ylim([.23 .31])
    set(gca, 'YLim', [0, get(gca, 'YLim') * [0; 1]])
    title (var{it}.title); 
    xlabel(var{it}.ylabel);
    ylabel('Frequency');
    %xlim([0 7.5e5])
    %set(gca,'XTick',[-90,-45,0,45,90])
    %%set(gca,'YTick',[-1,-0.5,0,0.5,1])
    %%set(gca,'XTick',linspace(1861,2011,150))
    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],figure_hdl);
    close(figure_hdl);
end
