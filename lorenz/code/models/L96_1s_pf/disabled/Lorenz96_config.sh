#!/usr/bin/env bash
#set -x
#=======================================================================
# Lorenz96 model with time periodic forcing
#=======================================================================

model_default_config(){
    funct_opening 2
    verity=$1
    echo "$verity Lorenz96_1s_pf config"

    export          dt="0.01d0"
    export      F_mean="8.0d0"
    export      F_ampl="0.0d0"
    export       F_tau="0.5d0" # Meaningless for F_ampl=0

    export update_mode="Augm1" #  'Hakim' or 'Augm1'
    export         SNR="10"
    export      detail="1"

    case "$verity" in
        0)
            export sampling_period=500

            export         nspinup=5
            export          cycles=50 #00
            export    cycle_length=5
            ;;
        1)
            export sampling_period=5000

            export         nspinup=50
            export          cycles=500 #00
            export    cycle_length=10
            ;;
        *) error "Unknown verity level $verity";;
    esac

    export     Taver_steps=$cycle_length

    funct_closing 2
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source ${COM_DAS_DIR}/common_das_tools.sh

model_default_config $@
