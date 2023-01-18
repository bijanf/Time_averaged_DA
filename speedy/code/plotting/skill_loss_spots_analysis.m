
% Response and growth rate functions

%-----------------------------------------------------------
% Script to plots of response and growth rate functions
% the first growth rate function is the one used by VS-Lite
% and the others are smoothed out versions I propose
%-----------------------------------------------------------
function skill_loss_spots_analysis()
    close all;

    %               -strange-- ---normal---
    figure %        poland  miami finland. alaska   
    obs_position = [  8 38; 73 34;  10 42;  56 43];
    obs_position_lat = -90 + ((obs_position(:,2)-1)/48)*180;
    obs_position_lon =   mod(((obs_position(:,1)-1)/96)*360 + 180,360)-180;
    
    title('Observation stations');
    
    m_proj('Equidistant cylindrical','long',[-180 180],'lat',[-90 90]);
    m_coast('color',[0 .6 0]);
    hold on
    m_plot(obs_position_lon,obs_position_lat,'o')
    m_grid('box','fancy','tickdir','in');

%     return
    for j=1:size(obs_position,1)
        composed_plot(obs_position(j,1),obs_position(j,2),num2str(j))
    end
    
end
function composed_plot(i,j,plot_title)

    
    file_path = '~/Desktop/nature_monthly_means.nc';
    nc_id1 = netcdf.open(file_path,'NOWRITE');
    
    var_name = 'temp0';
    var.id    = netcdf.inqVarID(nc_id1,var_name);
    var.(var_name)  = netcdf.getVar(nc_id1,var.id);
    
    var_name = 'swav';
    var.id    = netcdf.inqVarID(nc_id1,var_name);
    var.(var_name)  = netcdf.getVar(nc_id1,var.id);
    
    file_path = '~/Desktop/nature_monthly_means__min.nc';
    nc_id_min = netcdf.open(file_path,'NOWRITE');
    
    var_name = 'temp0';
    var.id    = netcdf.inqVarID(nc_id_min,var_name);
    temp0_min_array = netcdf.getVar(nc_id_min,var.id);
    
    var_name = 'swav';
    var.id    = netcdf.inqVarID(nc_id_min,var_name);
    soilw_min_array  = netcdf.getVar(nc_id_min,var.id);
    
    file_path = '~/Desktop/nature_monthly_means__max.nc';
    nc_id_max = netcdf.open(file_path,'NOWRITE');
    
    var_name = 'temp0';
    var.id    = netcdf.inqVarID(nc_id_min,var_name);
    temp0_max_array = netcdf.getVar(nc_id_max,var.id);
    
    var_name = 'swav';
    var.id    = netcdf.inqVarID(nc_id_max,var_name);
    soilw_max_array  = netcdf.getVar(nc_id_max,var.id);
    
    
    
    
%     i=8; j=38;
    stemp = squeeze(var.temp0(i,j,:));
    stemp_min = squeeze(temp0_min_array(i,j,:));
    stemp_max = squeeze(temp0_max_array(i,j,:));
    
    soilw = squeeze(var.swav(i,j,:));
    soilw_min = squeeze(soilw_min_array(i,j,:));
    soilw_max = squeeze(soilw_max_array(i,j,:));


figure


    zeta = (soilw./soilw) *20;
    plot3(stemp,soilw,zeta,':ko'); hold on;
%     plot3(stemp,soilw,zeta,':ko','LineStyle','none'); hold on;
    xlabel('T');ylabel('M');
%     refline(0,soilw_min);
%     refline(0,soilw_max);
view(0,90)




    
    
    cfg.T_resp_window = [ stemp_min stemp_max];
    cfg.M_resp_window = [ soilw_min soilw_max];
    
    cfg.T_range=cfg.T_resp_window; cfg.T = cfg.T_range(1):0.10:cfg.T_range(2);
    cfg.M_range=cfg.M_resp_window; cfg.M = cfg.M_range(1):0.01:cfg.M_range(2);
    
    cfg.line_levels = (0:10)*0.1;
    cfg.line_colors={'b','r'};
    
    cfg.hot_short=colormap('Hot');
    cfg.hot_short(end-10:end,:)=[];
    cfg.jet_short=colormap('jet');
    cfg.jet_short(1     : 10,:)=[];
    cfg.jet_short(end-10:end,:)=[];

    cfg.zero = zeros(length(cfg.M),length(cfg.T));
    
    cfg.title_on ='yes';
    cfg.title_on ='no';
    cfg.plot_formats  = {'-pdf','-eps'};
    
    cfg.membership_func={'ramp','sin'};


    % Response functions
    cfg.R.ramp.T = M_shoulder(cfg.T,cfg.T_resp_window(1),cfg.T_resp_window(2));
    cfg.R.ramp.M = M_shoulder(cfg.M,cfg.M_resp_window(1),cfg.M_resp_window(2));
    cfg.R.ramp.T_grid = repmat(cfg.R.ramp.T ,[length(cfg.M) 1]);
    cfg.R.ramp.M_grid = repmat(cfg.R.ramp.M',[1 length(cfg.T)]);

    cfg.R.sin.T = M_cosine(cfg.T,cfg.T_resp_window(1),cfg.T_resp_window(2));
    cfg.R.sin.M = M_cosine(cfg.M,cfg.M_resp_window(1),cfg.M_resp_window(2));
    cfg.R.sin.T_grid = repmat(cfg.R.sin.T ,[length(cfg.M) 1]);
    cfg.R.sin.M_grid = repmat(cfg.R.sin.M',[1 length(cfg.T)]);
     

    %     plots_1D(cfg);
    plots_3D(cfg);
    title(plot_title)
    %     plots_2D
    
    %     save_all_figs('gr_surface');

    %hold on;
    T_response = M_shoulder(stemp,cfg.T_resp_window(1),cfg.T_resp_window(2));
    M_response = M_shoulder(soilw,cfg.M_resp_window(1),cfg.M_resp_window(2));
    sum(min(T_response,M_response) - T_response.*M_response)
    
%         cfg.Gr = min(cfg.R.(mf).T_grid,cfg.R.(mf).M_grid) - ...
%             cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid;

    
    
    %
    % figure; plot(stemp);
    % figure; plot(soilw);
    
%     figure;


end

function plots_1D(cfg)
%     figure;
    hold on;
    for j=1:length(cfg.membership_func)
        mf=cfg.membership_func{j};
        
        plot(cfg.T, cfg.R.(mf).T,'b','LineWidth',2,'Color',cfg.line_colors{j});
    end
    xlabel('x');  ylabel('Response function'); ylim([-0.1 1.1]);
    legend(cfg.membership_func{:},'Location','Best');
end

function plots_3D(cfg)
    %
    %% Growth Rate functions

%     for j=1:length(cfg.membership_func)
%         mf=cfg.membership_func{j};
% 
%         cfg.M_func=mf;
%         
%         cfg.Tnorm = 'Minimum';
%         cfg.Gr = min(cfg.R.(mf).T_grid,cfg.R.(mf).M_grid);
%         cfg.title='Gr = min(R_T,R_M)';
%         surface_gr_3D(cfg)
% 
%         cfg.Tnorm = 'Product';
%         cfg.Gr = cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid;
%         surface_gr_3D(cfg)
% 
%         cfg.Tnorm = 'Lukasiewicz';
%         cfg.Gr = max(cfg.zero, cfg.R.(mf).T_grid + cfg.R.(mf).M_grid -1);
%         surface_gr_3D(cfg);
% 
%         cfg.Tnorm = 'Yager';
%         cfg.Gr = max(cfg.zero, 1 - ((1-cfg.R.(mf).T_grid).^2 + (1-cfg.R.(mf).M_grid).^2).^0.5);
%         surface_gr_3D(cfg);
% 
%         cfg.Tnorm = 'Hamacher';
%         cfg.Gr = (cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid)./...
%             (cfg.R.(mf).T_grid+cfg.R.(mf).M_grid -(cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid));
%         surface_gr_3D(cfg);
%     end
% 
%     %~ % other alternatives proposed by Estanislao
    %~ Gr{4}(i1,i2) = (response_funct_Smooth_cos(T, 0,1)*response_funct_Smooth_cos(M, 0,1))^(1/2);
    %~ Gr{5}(i1,i2) = (response_funct_Smooth_cos(T, 0,1)*response_funct_Smooth_cos(M, 0,1))^(1/3);
    %~ diff = abs(response_funct_Smooth_cos(T, 0,1)-response_funct_Smooth_cos(M, 0,1));
    %~ Gr5(i1,i2) = diff*Gr{3}(i1,i2)+(1-diff)*Gr{4}(i1,i2);
    %~ Gr6(i1,i2) = diff^2*Gr{3}(i1,i2)+(1-diff^2)*Gr{4}(i1,i2);

        %
    %% Growth Rate differences

    for j=1:1%length(cfg.membership_func)
        mf=cfg.membership_func{j};

        cfg.M_func=mf;
        
        cfg.Tnorm = 'Minimum';
        cfg.Gr = min(cfg.R.(mf).T_grid,cfg.R.(mf).M_grid) - ...
            cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid;
        
        cfg.title='Gr = min(R_T,R_M)';
        surface_gr_3D(cfg)

%         cfg.Tnorm = 'Product';
%         cfg.Gr = 
%         surface_gr_3D(cfg)
% 
%         cfg.Tnorm = 'Lukasiewicz';
%         cfg.Gr = max(cfg.zero, cfg.R.(mf).T_grid + cfg.R.(mf).M_grid -1);
%         surface_gr_3D(cfg);
% 
%         cfg.Tnorm = 'Yager';
%         cfg.Gr = max(cfg.zero, 1 - ((1-cfg.R.(mf).T_grid).^2 + (1-cfg.R.(mf).M_grid).^2).^0.5);
%         surface_gr_3D(cfg);
% 
%         cfg.Tnorm = 'Hamacher';
%         cfg.Gr = (cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid)./...
%             (cfg.R.(mf).T_grid+cfg.R.(mf).M_grid -(cfg.R.(mf).T_grid .* cfg.R.(mf).M_grid));
%         surface_gr_3D(cfg);
    end

end

function surface_gr_3D(cfg)
%     hdl = figure;
    set(gcf, 'PaperPosition', [0 0 6 4.5]);
    set(gcf, 'PaperSize'    , [6 4.5]);
%     set(gcf, 'PaperPosition', [0 0 4 3]);
%     set(gcf, 'PaperSize'    , [4 3]);
    hold on;
    
    surf   (cfg.T, cfg.M, cfg.Gr,'EdgeColor', 'none', 'LineStyle', 'none');
    contour(cfg.T, cfg.M, cfg.Gr,cfg.line_levels,'k');%,'LineWidth',0.5);
    colormap(cfg.jet_short);

    isequal(cfg.title_on,'yes') && title([cfg.M_func ', ' cfg.Tnorm]);
    
    xlabel('T (K)'); ylabel('M (v/v)'); zlabel('Growth Rate');
%     view([-18 54]);
%     lightangle(-110,54);
    xlim(cfg.T_range); ylim(cfg.M_range);
    
%     set(findobj(gca,'type','surface'),...
%         'FaceLighting','phong','AmbientStrength',.6,'DiffuseStrength',.8,...
%         'SpecularStrength',.9,'SpecularExponent',25,'BackFaceLighting','unlit')

%     text(-5,0.7,0.10,'(i)'  );
%     text(30,0.8,1.10,'(ii)' );
%     text(30,0.4,0.55,'(iii)');
%     text(12,0.75,0.65,'(iv)' );
%     hpt=patch([5,5,25,25], [0.3,0.7,0.7,0.3], [0,0,0,0], 'g');
%     hh1 = hatchfill(hpt, 'cross', 0, 10);
%     set(hpt, 'LineStyle', 'none')
    colorbar
    
    if ~ isequal(cfg.Tnorm,'Minimum')
       text(11,0.2,0.95,'(v)');
    end
    
%     filename = ['Gr_surf__' cfg.M_func '_' cfg.Tnorm];
%     saveas(hdl, filename,'fig') %Matlab .FIG file
%     export_fig(cfg.plot_formats{:},filename,hdl);
% print(hdl,'-depsc',filename); 
% print(hdl,'-dpdf',filename); 

% saveas(hdl, filename,'epsc') %pdf
%     saveas(hdl, filename,'pdf') %pdf

end

function plots_2D(cfg)
    figure
    
    subplot(2,2,1);hold on;
    surf(T_vect,M_vect,Gr{1}','EdgeColor', 'none', 'LineStyle', 'none','FaceLighting','phong');
    contour3(T_vect,M_vect,Gr{1}','k');colormap(jet_short);
    title('Gr = min(resp_1(x),resp_1(y))'); view(2);
    xlabel('T(C.deg.)'); ylabel('M(v/v)');axis tight;
    
    subplot(2,2,2);hold on;
    surf(T_vect,M_vect,Gr{2}','EdgeColor', 'none', 'LineStyle', 'none','FaceLighting','phong');
    contour3(T_vect,M_vect,Gr{2}','k');colormap(jet_short);
    title('Gr = resp_1(x)*resp_1(y)'); view(2);
    xlabel('T(C.deg.)'); ylabel('M(v/v)');axis tight;
    
    subplot(2,2,3);hold on;
    surf(T_vect,M_vect,Gr{3}','EdgeColor', 'none', 'LineStyle', 'none','FaceLighting','phong');
    contour3(T_vect,M_vect,Gr{3}','k');colormap(jet_short);
    title('Gr = min(resp_2(x),resp_2(y))'); view(2);
    xlabel('T(C.deg.)'); ylabel('M(v/v)');axis tight;
    
    subplot(2,2,4);hold on;
    surf(T_vect,M_vect,Gr{4}','EdgeColor', 'none', 'LineStyle', 'none','FaceLighting','phong');
    contour3(T_vect,M_vect,Gr{4}','k');colormap(jet_short);
    title('Gr = resp_2(x)*resp_2(y)'); view(2);
    xlabel('T(C.deg.)'); ylabel('M(v/v)'); axis tight;
    
    
    save_all_figs('gr_surface');
end
