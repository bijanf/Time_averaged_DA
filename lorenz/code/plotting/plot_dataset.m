clear all; close all
CODE_DIR = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting']);
startup

[status, dataset_dir] = system('cd ..; pwd');
cfg = get_cfg([strtrim(dataset_dir) '/config']);
addpath([CODE_DIR '/models/' strtrim(cfg.model{:})]);

switch cfg.dataset_kind{1}
    case 'run'
      run_plot(cfg);
    case 'run_set'
      run_set_plot(cfg);
    case 'run_set_bunch'        
      cfg_free = get_cfg(...
          [strtrim(dataset_dir) '/' cfg.rel_run_free_set_dir{:} '/config']);

      if ~ isfield(cfg,'set_par1_name')
          cfg.set_par1_name = {'Taver_length'};
      end
      
      if isequal(cfg.set_dim,2)
          if isfield(cfg_free,'par1_name') && isfield(cfg,'set_par1_name')
              % set_par1 becomes set_par1
              cfg.set_par2_name  = cfg.set_par1_name;
              cfg.set_par2_label = cfg.set_par1_label;
              cfg.set_par2_span  = cfg.set_par1_span;
              
              %  free_run par1 becomes set_par2
              cfg.set_par1_name  = cfg_free.par1_name;
              cfg.set_par1_label = cfg_free.par1_label;
              cfg.set_par1_span  = cfg_free.par1_span;
          end
      end
      cfg
      run_set_bunch_plot(cfg);
end
