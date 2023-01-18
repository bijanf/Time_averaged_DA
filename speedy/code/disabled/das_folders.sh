#!/usr/bin/env bash
export model="speedy"

echo " - Folder structure"

#DAS_DIR=$CODE_DIR/das_dir
MODEL_DIR=$CODE_DIR/speedy_tools

if [[ -n ${DAS_INITIALIZED:-} ]];then
    remove_from_PATH $BIN_DIR
    remove_from_PATH $CODE_DIR/running_scripts
    remove_from_PATH $CODE_DIR/speedy_tools
    remove_from_PATH $CODE_DIR/obs
fi
export PATH=$BIN_DIR:$CODE_DIR/speedy_tools:$CODE_DIR/running_scripts:$CODE_DIR/obs:$PATH


export ARCH_THERE="/scratch/acevedo/speedy-das_copies"

TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-speedy_copy${count}"; }

echo " - Setting up MPI " #(Machine dependent)
case $(hostname) in
    tux04|tux21|calc02)
  setup_modules
  #export MODULEPATH=/net/opt/system/modules/3.2.8/modulefiles
  #/net/opt/system/modules/3.2.8/bin/modulecmd bash load modules
#        setup_modules_system
        case "$F90" in
            "gfortran") module load mpich2/gfortran-4.4.5/1.3.1;;
            "ifort"   ) module load mpich2/ifort-12.0.2/1.3.1;;
            *         ) error "Unknown compiler $F90";;
        esac
        cp $CODE_DIR/speedy_tools/mpd.conf ~/.mpd.conf # Machine config file
        chmod 600 ~/.mpd.conf
        mpdboot -n 1
        echo "   Running in $(mpdtrace)"
        ;;
    calc01|calc03|calc04)
  setup_modules
#        source /net/opt/system/modules/default/init/bash
        module unload python/2.7.5
        module load ncl/6.1.2
        case "$F90" in
            "gfortran") module load mpich/3.0.4/gfortran472;;
            "ifort"   ) module load mpich2/ifort-12.0.2/1.3.1;;
            *         ) error "Unknown compiler $F90";;
        esac
        cp $CODE_DIR/speedy_tools/mpd.conf ~/.mpd.conf # Machine config file
        chmod 600 ~/.mpd.conf
        mpdboot -n 1
        echo "   Running in $(mpdtrace)"
        ;;
    soroban|node???)
        module load mpich2/ge/gcc/64/1.4.1p1
        ;;
    *) error " unknown machine";;
esac


#=========================================================================
#> @brief Compile all fortran programs for speedy model
#=========================================================================
das_make_all(){
    funct_opening 3

    echo "============================================"
    echo " Compiling das fortran programs"
    echo "============================================"

    echo " - speedy"
    speedy_build > /dev/null
     #make_speedy.sh              > /dev/null

    echo " - time_increment"
    time_increment_build.sh $BIN_DIR > /dev/null

    echo " - Emean_spread"
    # Emean_spread_build.sh > /dev/null
    build_program_parallel.sh $CODE_DIR/speedy_tools/Emean_spread.f90 $BIN_DIR > /dev/null

    echo " - clean_obs"
    build_program.sh $CODE_DIR/obs/clean_obs.f90 $BIN_DIR > /dev/null

    echo " - sully_obs"
    build_program.sh $CODE_DIR/obs/sully_obs.f90 $BIN_DIR > /dev/null

    echo " - dump_obs"
    build_program.sh $CODE_DIR/obs/dump_obs.f90 $BIN_DIR > /dev/null

    echo " - letkf"
    build_program_parallel.sh $CODE_DIR/letkf/letkf.f90 $BIN_DIR > /dev/null

    echo " - add states"
    build_program.sh $CODE_DIR/speedy_tools/add_states.f90 $BIN_DIR > /dev/null

    # create_observations_build.sh > /dev/null
    # obsmake_build.sh             > /dev/null

    # echo " - variance"
    # calculate_variance_build.sh  > /dev/null

    # echo " - difference"
    # make_calculate_difference.sh> /dev/null

    # echo " - Emean"
    # build_program $CODE_DIR/tools/Emean.f90 > /dev/null


    echo "---------------------------------------------"
    echo " All fortran programs compiled successfully"
    echo "============================================="

    funct_closing 3
}
