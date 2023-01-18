cfg.Tkind        = {'Taver','Insta'};
%cfg.Tkind        = {'Taver'};
%cfg.Tkind        = {'Taver','Insta','Tanom'};
cfg.Ephase       = {'prior','postr'};

cfg.var_name     = {'t','u','v','q'};
cfg.var_label.u  = 'u-wind';
cfg.var_label.v  = 'v-wind';
cfg.var_label.t  = 'Temperature';
cfg.var_label.q  = 'Humidity';
cfg.var_label.ps = 'Surface Pressure';

cfg.hor_stat.ps  = 'selname,ps';
cfg.hor_stat.q   = 'selname,q_sellevidx,2';
cfg.hor_stat.t   = 'selname,t_sellevidx,1';
cfg.hor_stat.u   = 'selname,u_sellevidx,5';
cfg.hor_stat.v   = 'selname,v_sellevidx,5';
                       
cfg.hor_label.ps = '';
cfg.hor_label.q  = '0.850 Sigma Level';
cfg.hor_label.t  = '0.925 Sigma Level';
cfg.hor_label.u  = '0.300 Sigma Level';
cfg.hor_label.v  = '0.300 Sigma Level';
% cfg.hor_label.q  = '850 hPa level';
% cfg.hor_label.t  = '925 hPa level';
% cfg.hor_label.u  = '300 hPa level';
% cfg.hor_label.v  = '300 hPa level';

cfg.var_unit.timstd1.u = 'm/s';
cfg.var_unit.timstd1.v = 'm/s';
cfg.var_unit.timstd1.t = 'K';
cfg.var_unit.timstd1.q = 'kg/kg';
cfg.var_unit.timmean   = cfg.var_unit.timstd1;

cfg.spatial_stats = {...
    'selname,ps',...
    'selname,q_sellevidx,2',...
    'selname,t_sellevidx,1',...
    'selname,u_sellevidx,5',...
    'selname,v_sellevidx,5'...
    };
cfg.stats         = {...
    'timstd1_vertmean',...
    'timstd1_zonmean'...
    };

cfg.mean_v.label        = 'Time Mean';
cfg.mean_v.color_axis.u = [-30  30];
cfg.mean_v.color_axis.v = [-10  10];
cfg.mean_v.color_axis.t = [250 295];
%cfg.mean_v.color_axis.q = [0  0.012];
cfg.mean_v.color_axis.q = [0  0.01];
cfg.mean_v.color_map_.t = 'Spectral10i';
cfg.mean_v.color_map_.q = 'PuBuGn8';
%cfg.mean_v.color_map_.q = 'Spectral10';
%cfg.mean_v.color_map_.q = 'Bu_10';
cfg.mean_v.color_map_.u = 'BuDRd_18i';
cfg.mean_v.color_map_.v = 'BuDRd_18i';

cfg.spread.label        = 'Spread';
cfg.spread.color_axis.u = [0  2.5];
cfg.spread.color_axis.v = [0  2.5];
cfg.spread.color_axis.t = [0  1];
cfg.spread.color_axis.q = [0 0.00022];
%cfg.spread.color_axis.q = [0 0.00025];
cfg.spread.color_map_.t = 'Accent7';
%  cfg.spread.color_map_.t = 'Accent8';
%  cfg.spread.color_map_.t = 'Spectral10i';
cfg.spread.color_map_.q = cfg.spread.color_map_.t;
cfg.spread.color_map_.u = cfg.spread.color_map_.t;
cfg.spread.color_map_.v = cfg.spread.color_map_.t;


cfg.skewness.label        = 'Skewness';
cfg.skewness.color_axis.u = [-3  3];
cfg.skewness.color_axis.v = [-3  3];
cfg.skewness.color_axis.t = [-12 0];
cfg.skewness.color_axis.q = [-3  1];
cfg.skewness.color_map_.t = 'BuOrR_14';
cfg.skewness.color_map_.q = cfg.skewness.color_map_.t;
cfg.skewness.color_map_.u = cfg.skewness.color_map_.t;
cfg.skewness.color_map_.v = cfg.skewness.color_map_.t;

cfg.RMSErr              = cfg.spread;
cfg.RMSErr.label        = 'RMSE';

cfg.StdDev              = cfg.spread;
cfg.StdDev.label        = 'Standard Deviation';

%----------------------------
% Absolute comparisons
%----------------------------
cfg.ErrRed.label        = 'Error Reduction';
cfg.ErrRed.color_axis.u = [-1 1]*5;
cfg.ErrRed.color_axis.v = [-1 1]*5;
cfg.ErrRed.color_axis.t = [-1 1]*1;
cfg.ErrRed.color_axis.q = [-1 1]*0.0001;
cfg.ErrRed.color_map_.t = 'BuDRd_12i';
cfg.ErrRed.color_map_.q = cfg.ErrRed.color_map_.t;
cfg.ErrRed.color_map_.u = cfg.ErrRed.color_map_.t;
cfg.ErrRed.color_map_.v = cfg.ErrRed.color_map_.t;

cfg.ErrInc.label        = 'Error Increase';
cfg.ErrInc.color_axis.u = [-1 1]*5;
cfg.ErrInc.color_axis.v = [-1 1]*5;
cfg.ErrInc.color_axis.t = [-1 1]*0.5;
cfg.ErrInc.color_axis.q = [-1 1]*0.00008;
cfg.ErrInc.color_map_.t = 'PiYG10i';
%cfg.ErrInc.color_map_.t = 'BuDRd_12';
%cfg.ErrInc.color_map_.t = 'BuDRd_18';
cfg.ErrInc.color_map_.q = cfg.ErrInc.color_map_.t;
cfg.ErrInc.color_map_.u = cfg.ErrInc.color_map_.t;
cfg.ErrInc.color_map_.v = cfg.ErrInc.color_map_.t;

cfg.var_unit.timstd1_reduc                    = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseReffree_prior    = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseReffree_prior    = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseRefNormT_prior   = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseRefNormT_prior   = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseRefNormAdd_prior = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseRefNormAdd_prior = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseReffree_postr    = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseReffree_postr    = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseRefNormT_postr   = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseRefNormT_postr   = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_DecreaseRefNormAdd_postr = cfg.var_unit.timstd1;
cfg.var_unit.timstd1_IncreaseRefNormAdd_postr = cfg.var_unit.timstd1;

%------------------------------------------------------
% Relative comparisons (very noisy in low error areas)
%------------------------------------------------------
% cfg.ErrRed.label        = 'Error Reduction';
% cfg.ErrRed.color_axis.u = [-70 70];
% cfg.ErrRed.color_axis.v = cfg.ErrRed.color_axis.u;
% cfg.ErrRed.color_axis.t = cfg.ErrRed.color_axis.u;
% cfg.ErrRed.color_axis.q = cfg.ErrRed.color_axis.u;
% %cfg.ErrRed.color_map_.t = 'BuDRd_12';
% cfg.ErrRed.color_map_.t = 'BuDRd_18i';
% cfg.ErrRed.color_map_.q = cfg.ErrRed.color_map_.t;
% cfg.ErrRed.color_map_.u = cfg.ErrRed.color_map_.t;
% cfg.ErrRed.color_map_.v = cfg.ErrRed.color_map_.t;
% 
% cfg.ErrInc.label        = 'Error Increment';
% cfg.ErrInc.color_axis.u = [-100 100];
% cfg.ErrInc.color_axis.v = cfg.ErrInc.color_axis.u;
% cfg.ErrInc.color_axis.t = cfg.ErrInc.color_axis.u;
% cfg.ErrInc.color_axis.q = cfg.ErrInc.color_axis.u;
% %cfg.ErrInc.color_map_.t = 'BuDRd_12';
% cfg.ErrInc.color_map_.t = 'BuDRd_18';
% cfg.ErrInc.color_map_.q = cfg.ErrInc.color_map_.t;
% cfg.ErrInc.color_map_.u = cfg.ErrInc.color_map_.t;
% cfg.ErrInc.color_map_.v = cfg.ErrInc.color_map_.t;

% cfg.var_unit.timstd1_reduc= struct('u','%','v','%','t','%','q','%');
%------------------------------------------------------

cfg.var_unit.timstd1_incre       = cfg.var_unit.timstd1_reduc;
cfg.var_unit.timstd1_ReducRefMin = cfg.var_unit.timstd1_reduc;

cfg.lim.RMSE.u = [0  10];
cfg.lim.RMSE.v = [0  10];
cfg.lim.RMSE.t = [0  4];
cfg.lim.RMSE.q = [0  0.002];

% old stuff

% cfg.time_stats    = {'timstd1'};

% cfg.label.u             ='U-Wind';
% cfg.label.v             ='V-Wind';
% cfg.label.t             ='Temperature';
% cfg.label.q             ='Humidity';
% cfg.label.timstd1       ='RMSE';
% cfg.label.timmean       ='Time Mean';
% cfg.label.timstd1_reduc ='Error Reduction';
% cfg.label.timstd1_incre ='Error Increment';
% cfg.label.timstd1_reducRefMin ='Error Reduction';

% cfg.stat_label.timstd1       ='RMSE';
% cfg.stat_label.timmean       ='Time Mean';
% cfg.stat_label.timstd1_reduc = 'Error Reduction');
% cfg.stat_label    = struct('timstd1','RMSE','timmean','Time Mean',...
%                              'reduc','Error Reduction');

% 
% cfg.color_map.timmean.t     = 'Spectral10';
% % cfg.color_map.timmean.t     = 'BuOrR_14i';
% cfg.color_map.timmean.q     = 'Bu_10i';
% cfg.color_map.timmean.u     = 'BuDRd_18';
% cfg.color_map.timmean.v     = 'BuDRd_18';
% 
% cfg.color_axis.Taver.timmean.u = [-30  30];
% cfg.color_axis.Taver.timmean.v = [-10  10];
% cfg.color_axis.Taver.timmean.t = [250 295];
% cfg.color_axis.Taver.timmean.q = [0  0.01];
% 
% 
% 
% cfg.color_axis.Insta.timmean   = cfg.color_axis.Taver.timmean;
% cfg.color_axis.Tanom.timmean   = cfg.color_axis.Taver.timmean;
% 
% 
% 
% 
% cfg.color_map.timstd1.t     = 'Spectral10';
% cfg.color_map.timstd1.q     = 'Spectral10';
% cfg.color_map.timstd1.u     = 'Spectral10';
% cfg.color_map.timstd1.v     = 'Spectral10';
% 
% cfg.color_map.timstd1_reduc.t = 'BuOr_12';
% cfg.color_map.timstd1_reduc.q = 'BuOr_12';
% cfg.color_map.timstd1_reduc.u = 'BuOr_12';
% cfg.color_map.timstd1_reduc.v = 'BuOr_12';
% 
% cfg.color_map.timstd1_incre       = cfg.color_map.timstd1_reduc;
% cfg.color_map.timstd1_reducRefMin = cfg.color_map.timstd1_reduc;
% 
% % %cfg.color_map.timmean       = 'BuOr_12';
% % % cfg.color_map.timmean       = 'Spectral10';
% % cfg.color_map.timstd1       = 'Spectral10';
% % cfg.color_map.timstd1_reduc = 'BuOr_12';
% % cfg.color_map.timstd1_incre = 'BuOr_12';
% % cfg.color_map.timstd1_reducRefMin = 'BuOr_12';
% 
% 
% % c_map = colormap(othercolor('Spectral10')); colormap(flipud(c_map));
% %     colormap(othercolor('BuOr_12'));
% %     colormap(othercolor('RdYlGn4'));
% 
% cfg.color_axis.Taver.timstd1.u = [0  2];
% cfg.color_axis.Taver.timstd1.v = [0  2];
% cfg.color_axis.Taver.timstd1.t = [0  1];
% cfg.color_axis.Taver.timstd1.q = [0 0.0002];
% 
% cfg.color_axis.Insta.timstd1.u = [0  15];
% cfg.color_axis.Insta.timstd1.v = [0  15];
% cfg.color_axis.Insta.timstd1.t = [0   1.5];
% cfg.color_axis.Insta.timstd1.q = [0 0.00015];
% 
% cfg.color_axis.Tanom = cfg.color_axis.Insta;
% 
% 
% cfg.color_axis.Taver.timstd1_reduc.u = [-50 50];
% cfg.color_axis.Taver.timstd1_reduc.v = [-50 50];
% cfg.color_axis.Taver.timstd1_reduc.t = [-50 50];
% cfg.color_axis.Taver.timstd1_reduc.q = [-50 50];
% 
% cfg.color_axis.Insta.timstd1_reduc = cfg.color_axis.Taver.timstd1_reduc;
% cfg.color_axis.Tanom.timstd1_reduc = cfg.color_axis.Taver.timstd1_reduc;
% 
% cfg.color_axis.Taver.timstd1_incre = cfg.color_axis.Taver.timstd1_reduc;
% cfg.color_axis.Insta.timstd1_incre = cfg.color_axis.Taver.timstd1_reduc;
% cfg.color_axis.Tanom.timstd1_incre = cfg.color_axis.Taver.timstd1_reduc;
% 
% cfg.color_axis.Taver.timstd1_reducRefMin.u = [-50 50];
% cfg.color_axis.Taver.timstd1_reducRefMin.v = [-50 50];
% cfg.color_axis.Taver.timstd1_reducRefMin.t = [-50 50];
% cfg.color_axis.Taver.timstd1_reducRefMin.q = [-50 50];
% 
% cfg.color_axis.Insta.timstd1_reducRefMin = ...
%     cfg.color_axis.Taver.timstd1_reduc;
% 
% % cfg.color_axis.nature.u.timmean = [0  15];
% % cfg.color_axis.nature.v.timmean = [0  15];
% % cfg.color_axis.nature.t.timmean = [0  10];
% % cfg.color_axis.nature.q.timmean = [0 0.002];
% 
% % cfg.color_axis.free.u.timmean = [0  15];
% % cfg.color_axis.free.v.timmean = [0  15];
% % cfg.color_axis.free.t.timmean = [0   5];
% % cfg.color_axis.free.q.timmean = [0 0.0015];
% 
% % cfg.color_axis.nature.u.reduc = [-100 100];
% % cfg.color_axis.nature.v.reduc = [-100 100];
% % cfg.color_axis.nature.t.reduc = [-100 100];
% % cfg.color_axis.nature.q.reduc = [-100 100];
% 
% %cfg.color_axis.free = cfg.color_axis.nature;
% %cfg.color_axis.assi = cfg.color_axis.free;
% 
