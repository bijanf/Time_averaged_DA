;==============================================================
;
;==============================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; - Read in data
n_comp      =  asciiread(cfg_dir +         "/n_comp.cfg",-1,"integer")
dataset_dim =  asciiread(cfg_dir +    "/dataset_dim.cfg",-1,"integer")
var_name    = (/"comp1","comp2"/) ; + sprinti("%1.1i",icomp)
float_nan   = -9.99e+33    ; CDO default
; float_nan   =  9.96921e+36 ; NCL default: 

stat_cycles_values = readAsciiTable(cfg_dir + "/stat_cycles_values.cfg", 1,"integer",0)
stat_dirs          = readAsciiTable(cfg_dir + "/stats_dirs.cfg", 1,"string",0)
stat_cycles_span_length = max(dimsizes(stat_cycles_values))

line_nc     = addfile(scalar_name + ".nc","c")
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

fluctuation  = new( (/n_comp, stat_cycles_span_length - 1 /),"float")
do i = 1,stat_cycles_span_length-1
  do icomp = 0, n_comp-1
    if(scalar(icomp,i-1) .eq. 0) then
      fluctuation(icomp,i-1) = float_nan
    else
      fluctuation(icomp,i-1) = 100* abs((scalar(icomp,i)-scalar(icomp,i-1))/scalar(icomp,i-1))
    end if
  end do
end do

; - Add attributes
fluctuation!1                     = "stat_cycles"
fluctuation&stat_cycles           = stat_cycles_values(1:,0)
fluctuation&stat_cycles@long_name = "Assimilation cycles"
fluctuation@long_name             = "Stat. Fluctuations"
fluctuation@units                 = "%"
fluctuation@_FillValue            = float_nan

fluct_line_nc   = addfile(scalar_name + "_fluct.nc","c")
do icomp = 0, n_comp-1
  fluct_line_nc->$var_name(icomp)$ =  fluctuation(icomp,:)
end do

exit
