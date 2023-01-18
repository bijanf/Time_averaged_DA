#!/usr/bin/env bash
#=======================================================================
#> @brief For deleting links of common sources
#=======================================================================
unlink_common()
{
	rm -f SFMT.f90
	rm -f common.f90
	rm -f common_mpi.f90
	rm -f common_obs.f90
	rm -f common_mtx.f90
	rm -f common_letkf.f90
	rm -f netlib.f
	
	rm -f common_speedy.f90
	rm -f common_mpi_speedy.f90
	rm -f common_obs_speedy.f90
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

unlink_common $@
exit $?

