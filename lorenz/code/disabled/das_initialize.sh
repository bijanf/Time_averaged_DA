#!/usr/bin/env bash
das_initialize(){
    echo "======================================"
    echo " Initializing DAS"
    echo "======================================"

    # Go to folder where this file lies
    HERE=$( dirname "${BASH_SOURCE[0]}" ); cd "$HERE"
		
    echo " - Setting das environment variables"

    #--- machine independent variables ----
    export LANG=C # no localization features
    
    export        CODE_DIR=$(pwd);
    export         BIN_DIR=$CODE_DIR/../bin
    export         COM_DIR=$CODE_DIR/common
    export         DAS_DIR=$CODE_DIR/das_dir
    export NCARG_COLORMAPS=$DAS_DIR/ncl_colormaps

    echo " - Updating path"
    export PATH=$BIN_DIR:$DAS_DIR:$PATH
    
    #--- machine dependent variables ----
    echo " - Running in $(hostname)"
    case "$(hostname)" in
        negrito)
	    cpus=2
	    ARCH_DIR=$CODE_DIR/../data           ;;
        tux04  )
	    cpus=4
	    ARCH_DIR=/daten/model/acevedo/das_archive ;;
#	    ARCH_DIR=/scratch/users/acevedo/archive/das_archive ;;
        calc02 )
	    cpus=2 # calc2 has 8 cpus but normally quite bussy
	    ARCH_DIR=/daten/model/acevedo/das_archive ;;
#	    ARCH_DIR=/bigtmp/acevedo/das_archive ;;
        calc03|calc04 )
	    cpus=8
	    ARCH_DIR=/daten/model/acevedo/das_archive
            source /net/opt/system/modules/default/init/bash
	    module unload python/2.7.5
	    module load ncl/6.1.2
         ;;
        soroban|node???) cpus=8; ARCH_DIR=/scratch/acevedo
            module load netcdf/gcc/64/4.1.1
	    ;;
        # calc01) # Too old bash
        #    a41) # No netcdf module
        *)  echo " unknown machine $(hostname)";          return 1      ;;
    esac
    export cpus 
    export  ARCH_DIR
    [ ! -d $ARCH_DIR ] && mkdir -p $ARCH_DIR

    echo "--------------------------------------"
    echo " DAS enviroment properly set"
    echo "======================================"
}

set -o nounset # Unset variable expansion gives error
das_initialize $@
