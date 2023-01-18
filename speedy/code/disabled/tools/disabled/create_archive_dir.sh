#!/usr/bin/env bash
#=======================================================================
#> @brief Creates archive_dir or asks if it must be replaced 
#=======================================================================
create_archive_dir()
{
    [ $# -eq 1 ] || error "Usage: create_archive_dir archive_dir"
    archive_dir=$1

    if [ -d $archive_dir ]; then
        echo " There is already a dataset called $( basename $archive_dir )"
        read -p " Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $archive_dir; mkdir -p $archive_dir;;
                 * ) return 1;;
        esac
    else
        mkdir -p $archive_dir
    fi
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_archive_dir $@
exit $?
