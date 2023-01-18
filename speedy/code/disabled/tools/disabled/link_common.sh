#!/usr/bin/env bash
#=======================================================================
#> @brief Creates soft links to the common sources
#=======================================================================
link_common()
{
           COMMON=$DAS/common
    COMMON_SPEEDY=$DAS/speedy/common
    
    ln -fs $COMMON/SFMT.f90 ./
    ln -fs $COMMON/common.f90 ./
    ln -fs $COMMON/common_mpi.f90 ./
    ln -fs $COMMON/common_obs.f90 ./
    ln -fs $COMMON/common_mtx.f90 ./
    ln -fs $COMMON/common_letkf.f90 ./
    ln -fs $COMMON/netlib.f ./
    
    ln -fs $COMMON_SPEEDY/common_speedy.f90 ./
    ln -fs $COMMON_SPEEDY/common_mpi_speedy.f90 ./
    ln -fs $COMMON_SPEEDY/common_obs_speedy.f90 ./
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

link_common $@
exit $?

