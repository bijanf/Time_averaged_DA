function lon_lat_and_zonal_plot(var,plot_name,varargin)
    
    startup;
%     var.hor_stat
    
    figure_hdl = figure();
    set(figure_hdl,'Position', [0 0 1000 300]); clf;

    plot_width    = [0.505 0.15];   plot_height = 0.78;
    plot_ypos     =  0.14;   plot_xpos   = [0.1  0.62];
    
    %--------------------    
    % LON-LAT PLOT
    %--------------------    
    
    axes('pos',[plot_xpos(1) plot_ypos plot_width(1) plot_height]);
    hold on;

    
    

%     
%     m_proj('Equidistant cylindrical','long',[-180 180],'lat',[-90 90]);
%     m_coast('color',[0 .6 0]);
%     hold on

%     zeta = (obs_position_lon ./ obs_position_lon) *100;
%     plot3(obs_position_lon,obs_position_lat,zeta,':ko','LineStyle','none')

%     plot3(obs_position_lon,obs_position_lat,zeta,10,[.5 0 0],'filled')
%      scatter(obs_position_lon,obs_position_lat,10,[.5 0 0],'filled')

% %     m_grid('box','fancy','tickdir','in');
%     
% %     set(gcf, 'PaperPosition', [0 0 24 12]);
% %     print(gcf,'-dpsc2','-r0',[station_set '.eps'])
%     set(gca,'XTick'     ,[    -pi,  -pi/2,  0 , pi/2])
%     set(gca,'XTickLabel',{})
%     set(gca,'YTick'     ,[-2*pi/6, -pi/6,   0, pi/6, 2*pi/6])
%     set(gca,'YTickLabel',{})
%     ylim([-pi pi]/2); xlim([-pi pi]); box on;
% 
%     saveas(figure_hdl,[plot_name '.fig'],'fig');
%     export_fig('-transparent','-eps','-pdf',plot_name,figure_hdl);
%     close(figure_hdl);



    load coast
    
    lon_range = '-180_to_180';
    %lon_range = '0_to_360';
    
    hsurf_type = 'imagesc';
    %hsurf_type = 'm_contourf'; % Prone to yield artifacts for rough fields
    
    switch hsurf_type
        case 'imagesc'
            var.hor_stat.data = circshift(var.hor_stat.data,[48 0 0]);
            imagesc(...
                var.hor_stat.lon.data -180,...
                var.hor_stat.lat.data, ...
                var.hor_stat.data');
            line(long    ,lat,'color','k','linewidth',1)
            line(long-360,lat,'color','k','linewidth',1)


            axis tight; xlim([-180 180]);
            set(gca,'XTick'     ,[    -180,  -90,  0 , 90])
            set(gca,'XTickLabel',{ '-180',  '-90', '0', '90'})
            set(gca,'YTick'     ,[-60, -30,   0,  30, 60])
            set(gca,'YTickLabel',{  '-60', '-30', '0', '30',   '60'})

        case 'm_contourf'
            [latt lonn] = meshgrid(var.hor_stat.lat.data,var.hor_stat.lon.data);

            switch lon_range
                case '-180_to_180'
                    lonn = mod(circshift(lonn,[48 0 0]) + 180,360) -180;
                    var.hor_stat.data = circshift(var.hor_stat.data,[48 0 0]);
                    
                    lonn    (97,:) = 180;
                    latt    (97,:) = latt    (1,:);
                    var.hor_stat.data(97,:) = var.hor_stat.data(1,:);
                    
                    m_proj('Equidistant Cylindrical','long',[-180 180],'lat',[-88 88])
                    m_contourf(lonn,latt, var.hor_stat.data);
                    m_line(long    ,lat,'color','k','linewidth',1)
                    %     m_grid('grid','off','tickdir','in','fontsize',11,'linewidth',1,'fontweight','bold');
                case '0_to_360'
                    m_proj('Equidistant Cylindrical','long',[0 360],'lat',[-90 90])
                    m_contourf(lonn,latt, var.hor_stat.data);
                    m_line(long+360,lat,'color','k','linewidth',1)
                    m_line(long    ,lat,'color','k','linewidth',1)
            end
            set(gca,'XTick'     ,[    -pi,  -pi/2,  0 , pi/2])
            set(gca,'XTickLabel',{ '-180',  '-90', '0', '90'})
            set(gca,'YTick'     ,[-2*pi/6, -pi/6,   0, pi/6, 2*pi/6])
            set(gca,'YTickLabel',{  '-60', '-30', '0', '30',   '60'})

    end
    
    shading interp
    shading flat
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');

    file_path = '../raw_data/obs/station.tbl';
    if exist(file_path, 'file')
        try
            obs_position = dlmread(file_path','',2,0);
        catch
            obs_position =[NaN NaN];
        end
        obs_position_lat = -90 + ((obs_position(:,2)-1)/48)*180;
        obs_position_lon = mod(((obs_position(:,1)-1)/96)*360 + 180,360)-180;
        plot(obs_position_lon,obs_position_lat,':k+','MarkerSize',1,'LineStyle','none')
    end

    %--------------------------------------------------------------------
    % other untested possible plotting types
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
    %--------------------------------------------------------------------
     
    title(var.hor_label);
    xlabel('Longitude [ deg. ]'); ylabel('Latitude [ deg. ]');

    % Color scale
    
    caxis(var.color_axis);
    c_map = colormap(othercolor(var.color_map)); %colormap(flipud(c_map));
    
    
    
    %--------------------    
    %   ZONAL PLOT
    %--------------------    

    axes('pos',[plot_xpos(2) plot_ypos plot_width(2) plot_height]);
    hold on;
    
    hdl = imagesc(...
        var.zon_stat.lev.data,...
        var.zon_stat.lat.data,...
        squeeze(var.zon_stat.data));

    shading flat; 
    %    shading interp;
    set(gca,'layer','top');
    set(gcf,'renderer','zbuffer');
%    set(gca,'YDir','reverse');
    set(gca,'YDir','normal');


    % Color scale
    caxis(var.color_axis);
    c_map = colormap(othercolor(var.color_map));

    ylim([-90 90]); xlim([  0  1]);
    
%     title(var.zon_stat.label); 
    title('Zonal Mean');

%     ylabel(var.zon_stat.lat.label); 
%    xlabel(var.zon_stat.lev.label);
    xlabel('Sigma [ Pa/Pa ]');
    
    set(gca,'XTick',[0,0.5,1.0]);
    set(gca,'YTick',[])

    % Color scale
    hcb = colorbar;
    ylabel(hcb, [var.label ' ' var.stat_label ' ' '[ ' var.unit ' ]']);

    
    saveas(figure_hdl,[plot_name '.fig']);
    export_fig(plot_name,'-pdf','-eps','-transparent',figure_hdl);
 
    close(figure_hdl);
end
