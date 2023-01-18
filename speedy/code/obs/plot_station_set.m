function plot_station_set(station_set)

try
    obs_position = dlmread([station_set '.tbl']','',2,0);
catch
    obs_position =[NaN NaN];
end

obs_position_lat = -90 + ((obs_position(:,2)-1)/48)*180; 
obs_position_lon =   mod(((obs_position(:,1)-1)/96)*360 + 180,360)-180; 

title('Observation stations');

m_proj('Equidistant cylindrical','long',[-180 180],'lat',[-90 90]);
m_coast('color',[0 .6 0]);
hold on
m_plot(obs_position_lon,obs_position_lat,'o')
m_grid('box','fancy','tickdir','in');

set(gcf, 'PaperPosition', [0 0 24 12]);    
print(gcf,'-dpsc2','-r0',[station_set '.eps'])
quit force
