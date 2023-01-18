clear all; close all
CODE_DIR = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting']);

fprintf('\n');
startup

[status, dataset_dir] = system('cd ..; pwd');
cfg = get_cfg([strtrim(dataset_dir) '/config']);

metainfo_for_plots

%  [status, dataset_dir] = system('cd ..; pwd');
%  cfg = get_cfg([strtrim(dataset_dir) '/config']);

switch cfg.dataset_kind{1}
    case 'run'
      run_plot(cfg);
    case 'run_set'
      run_set_plot(cfg);
    case 'run_set_bunch'
      run_set_bunch_plot(cfg);
end
