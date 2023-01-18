;==============================================================
;
;==============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

float_nan   = -9.99e+33    ; CDO default
; float_nan   =  9.96921e+36 ; NCL default: 

; - Read in data
n_comp      =        asciiread(cfg_dir +         "/n_comp.cfg",-1,"integer")
dataset_dim =        asciiread(cfg_dir +    "/dataset_dim.cfg",-1,"integer")

if(dataset_dim.ge.1) then
  par1_pos    = readAsciiTable(cfg_dir + "/par1_value_pos.cfg", 1,"integer",0)
  par1_values = readAsciiTable(cfg_dir +    "/par1_values.cfg", 1,"float"  ,0)
  par1_name   = asciiread     (cfg_dir +      "/par1_name.cfg",-1,"string")
  par1_label  = asciiread     (cfg_dir +     "/par1_label.cfg",-1,"string")
  par1_span_length = max(dimsizes(par1_values))
end if
if(dataset_dim.ge.2) then
  par2_pos    = readAsciiTable(cfg_dir + "/par2_value_pos.cfg", 1,"integer",0)
  par2_values = readAsciiTable(cfg_dir +    "/par2_values.cfg", 1,"float"  ,0)
  par2_name   = asciiread     (cfg_dir +      "/par2_name.cfg",-1,"string")
  par2_label  = asciiread     (cfg_dir +     "/par2_label.cfg",-1,"string")
  par2_span_length = max(dimsizes(par2_values))
end if

scalar_list = readAsciiTable(scalar_name + ".list",n_comp,"float"  ,0)
list_length = max(dimsizes(scalar_list))
scalar_nc   = addfile(scalar_name + ".nc","c")

if(dataset_dim.eq.1) then
  scalar = new((/par1_span_length/),float)
end if
if(dataset_dim.eq.2) then
  scalar = new((/par1_span_length,par2_span_length/),float)
end if

do icomp = 1, n_comp
  var_name = "comp" + sprinti("%1.1i",icomp)

  do i = 0, list_length-1

    if(dataset_dim.eq.1) then
      scalar(par1_pos(i,0)) = scalar_list(i,icomp-1)
    end if
    if(dataset_dim.eq.2) then
      scalar(par1_pos(i,0),par2_pos(i,0)) = scalar_list(i,icomp-1)
    end if

  end do

; - Add attributes
  if(dataset_dim.ge.1) then
    scalar!0                      = par1_name
    scalar&$par1_name$            = par1_values(:,0)
    scalar&$par1_name$@long_name  = par1_label
    scalar&$par1_name$@_FillValue = float_nan
  end if
  if(dataset_dim.ge.2) then
    scalar!1                      = par2_name
    scalar&$par2_name$            = par2_values(:,0)
    scalar&$par2_name$@long_name  = par2_label
    scalar&$par2_name$@_FillValue = float_nan
  end if

	scalar@_FillValue            = float_nan

  scalar_nc->$var_name$ = scalar
end do

exit
