function lon_lat_plot(var,plot_name,varargin)
    
    startup;
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
    set(figure_hdl,'Position', [0 0 700 300]); clf;
    
    [latt lonn] = meshgrid(var.lat.data,var.lon.data);
    load coast
    
    lon_range = '-180_to_180';
    %lon_range = '0_to_360';
    
    switch lon_range
        case '-180_to_180'
            lonn = mod(circshift(lonn,[48 0 0]) + 180,360) -180;
            var.data = circshift(var.data,[48 0 0]);
            
            lonn    (97,:) = 180;
            latt    (97,:) = latt    (1,:);
            var.data(97,:) = var.data(1,:);
            
            m_proj('Equidistant Cylindrical','long',[-180 180],'lat',[-88 88])
            m_contourf(lonn,latt, var.data);
            m_line(long    ,lat,'color','k','linewidth',1)
        case '0_to_360'
            m_proj('Equidistant Cylindrical','long',[0 360],'lat',[-90 90])
            m_contourf(lonn,latt, var.data);
            m_line(long+360,lat,'color','k','linewidth',1)
            m_line(long    ,lat,'color','k','linewidth',1)
    end
    
    %shading interp
    shading flat

    % other untested plotting types
    % imagesc(var.lon.data, var.lat.data, squeeze(var.data)');

    % geoshow(latt, lonn, var.data,'DisplayType','texturemap');
    % % Display the land area boundary as black lines.
    % S = shaperead('landareas','UseGeoCoords',true);
    % geoshow([S.Lat], [S.Lon],'Color','black');

    % axesm miller; axis off; framem on; gridm on;
    % surfm(latt, lonn, double(var.data));
    
    % m_pcolor(lonn,latt, squeeze(var.data)); 
    % !!! Using m_pcolor not all the lines are plotted !!!
    % see http://meyavuz.blogspot.de/2011/12/difference-between-pcolor-and-imagesc.html

    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');
     
    if isfield(var,'color_axis')
        caxis(var.color_axis);
    end
     
    title(var.label);
    xlabel(var.lon.label); ylabel(var.lat.label);

    m_grid('grid','off','tickdir','in','fontsize',11,'linewidth',1,'fontweight','bold');
%     
%     set(gca,'XTick',[0,90,180,270,360])
%    set(gca,'YTick',[-90,-45,0,45,90])

    % Color scale
    
caxis(var.color_axis);
    c_map = colormap(othercolor('Spectral10')); colormap(flipud(c_map));
%     colormap(othercolor('BuOr_12'));
%     colormap(othercolor('RdYlGn4'));
%     c_map = colormap(darkb2r(var.color_axis(1),var.color_axis(2)));
%     colormap(flipud(c_map));
    hcb = colorbar;
    axis tight
    %ylabel(hcb, pretitle);
%     
     saveas(figure_hdl,[plot_name '.fig'],'fig');
     export_fig('-transparent',[plot_name '.pdf'],figure_hdl);
 
     close(figure_hdl);
end
