function [cfg] = get_cfg(cfg_dir)
  old_dir = cd(cfg_dir);
  cfg_row = dir;
  for i=3:length(cfg_row)
    cfg_file = cfg_row(i).name;
    [dummy,cfg_field,dummy] = fileparts(cfg_file);
    
    field_value = importdata(cfg_file);

    if (~ ismatrix(field_value))
        field_value = num2cell(field_value);
    end
        
    cfg.(cfg_field) = field_value;
    clear cfg_field;
  end
  cd(old_dir)
end