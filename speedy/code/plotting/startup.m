CODE_DIR = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting/export_fig']);
addpath([CODE_DIR '/plotting/othercolor']);
% addpath([CODE_DIR '/plotting']);
addpath([CODE_DIR '/common_scripts/my_tools']);

set(0,'DefaultAxesFontSize'   ,   16);
set(0,'DefaultLineLinewidth'  ,    2);
%set(0,'DefaultTextInterpreter','none');
%set(0,'DefaultTextInterpreter','Latex');
set(0,'DefaultTextInterpreter','Tex');
set(0,'DefaultAxesBox'        , 'on');

%addpath([CODE_DIR '/models/' strtrim(cfg.model{:})]);

CODE_DIR = getenv('CODE_DIR');
addpath([CODE_DIR '/plotting']);

