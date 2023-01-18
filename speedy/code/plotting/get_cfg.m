function [cfg] = get_cfg(cfg_dir)
  old_dir = cd(cfg_dir);
  cfg_row = dir;
  for i=3:length(cfg_row)
    cfg_file = cfg_row(i).name;
    [dummy,cfg_field,dummy] = fileparts(cfg_file);
    cfg.(cfg_field) = importdata(cfg_file);
  end
  cd(old_dir)
end