;==============================================================
;
;==============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; - Read in data
n_comp      =        asciiread(cfg_dir +         "/n_comp.cfg",-1,"integer")
dataset_dim =        asciiread(cfg_dir +    "/dataset_dim.cfg",-1,"integer")
var_name    = (/"comp1","comp2"/) ; + sprinti("%1.1i",icomp)
stat_cycles_values = readAsciiTable(cfg_dir + "/stat_cycles_values.cfg", 1,"integer",0)
stat_dirs          = readAsciiTable(cfg_dir + "/stats_dirs.cfg", 1,"string",0)
stat_cycles_span_length = max(dimsizes(stat_cycles_values))

line_nc   = addfile(scalar_name + ".nc","c")
scalar      = new( (/n_comp, stat_cycles_span_length/),"float")

do i = 0,stat_cycles_span_length-1
  stat_file = "../" + stat_dirs(i,0) + "/" + scalar_name + ".dat"
  scalar(:,i) = asciiread(stat_file,-1,"float")
end do
; print(scalar)

; - Add attributes
scalar!1                     = "stat_cycles"
scalar&stat_cycles           = stat_cycles_values(:,0)
scalar&stat_cycles@long_name = "Assimilation cycles"

do icomp = 0, n_comp-1
  line_nc->$var_name(icomp)$ =  scalar(icomp,:)
end do
; print(line_nc)

improvement  = new( (/n_comp, stat_cycles_span_length - 1 /),"float")
do i = 1,stat_cycles_span_length-1
  improvement(:,i-1) = 100* abs((scalar(:,i) - scalar(:,i-1)) / scalar(:,i-1))
end do

; - Add attributes
improvement!1                     = "stat_cycles"
improvement&stat_cycles           = stat_cycles_values(1:,0)
improvement&stat_cycles@long_name = "Assimilation cycles"
improvement@long_name = "Improvement"
improvement@units     = "%"

impr_line_nc   = addfile(scalar_name + "_impr.nc","c")
do icomp = 0, n_comp-1
  impr_line_nc->$var_name(icomp)$ =  improvement(icomp,:)
end do

exit
