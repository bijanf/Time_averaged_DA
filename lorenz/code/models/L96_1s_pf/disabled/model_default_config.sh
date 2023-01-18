#!/usr/bin/env bash
#set -x
#=======================================================================
# Lorenz96 model with time periodic forcing
#=======================================================================
model_default_config(){
    funct_opening 2
    verity=$1

    echo " Lorenz96_1s_pf config verity $verity"
    export sampling_size=20

    export          dt="0.01"
    export      F_mean="8.0d0"
    export      F_ampl="0.0d0"
    export       F_tau="1.0d0" # Meaningless for F_ampl=0

    export    cycle_length=$(echo 5*$dt|bc)
    case "$verity" in
        0)
            export sampling_period=500
            export         nspinup=5
            export          cycles=50 #00
            ;;
        1)
            export sampling_period=5000
            export         nspinup=50
            export          cycles=500 #00
            ;;
        *) error "Unknown verity level $verity";;
    esac

    
    export update_mode="Hakim" #  'Hakim' or 'Augm1'
    export      detail="1"



    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh
model_default_config $@