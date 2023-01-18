MODULE station_data
  IMPLICIT NONE
  INTEGER,PROTECTED :: j_pos
  INTEGER,PARAMETER :: ny = 40
  INTEGER,PARAMETER :: station_ind (ny)=(/(j_pos, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_pos (ny)=(/(j_pos, j_pos=1,ny)/)
  INTEGER,PARAMETER :: station_comp(ny)=(/(      0, j_pos=1,ny)/)
END MODULE station_data
