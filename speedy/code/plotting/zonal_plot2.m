function zonal_plot2(var,plot_name,varargin)

    %color_axis = [0 15];

    %Nvararg = length(varargin);
    %for i = 1:Nvararg/2
        %namein = varargin{2*i-1};
        %valin = varargin{2*i};
        %switch namein
            %case 'color_axis'
                %color_axis = valin;
            %otherwise
                %error(['Unknown ' mfilename ' parameter -> ' namein]);
        %end
    %end
    
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 300 300]); clf;
       
%    pcolor(var.lat.data, var.lev.data, squeeze(var.data)');
    imagesc(var.lev.data, var.lat.data, squeeze(var.data));

    shading flat; 
    %    shading interp;
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');
%    set(gca,'YDir','reverse');
    set(gca,'YDir','normal');

    if isfield(var,'color_axis')
        caxis(var.color_axis);
    end

    % Color scale
%     c_map = colormap(darkb2r(var.color_axis(1),var.color_axis(2)));
%     colormap(flipud(c_map));
caxis(var.color_axis);
    c_map = colormap(othercolor('Spectral10')); colormap(flipud(c_map));
%     colormap(othercolor('BuOr_12'));
%     colormap(othercolor('RdYlGn4'));
% 

    ylim([-90 90]); xlim([  0  1]);
    
    title(var.label); 
    ylabel(var.lat.label); xlabel(var.lev.label);
    
    set(gca,'XTick',[-90,-45,0,45,90]);
    %set(gca,'YTick',[-1,-0.5,0,0.5,1])

    % Color scale
    hcb = colorbar;
    %ylabel(hcb, pretitle);
    
    saveas(figure_hdl,[plot_name '.fig'],'fig');
    export_fig('-transparent',[plot_name '.pdf'],figure_hdl);

    close(figure_hdl);

end
