CODE_DIR = getenv('CODE_DIR');
if isequal(CODE_DIR,'')
    CODE_DIR = '..';
end
addpath([CODE_DIR '/plotting/export_fig']);
addpath([CODE_DIR '/plotting/othercolor']);

set(0,'DefaultTextFontSize', 16)
set(0,'DefaultAxesFontSize', 16);
set(0,'DefaultLineLinewidth'  ,    1);
%set(0,'DefaultTextInterpreter','none');
%set(0,'DefaultTextInterpreter','Latex');
set(0,'DefaultTextInterpreter','Tex');
set(0,'DefaultAxesBox'        , 'on');
