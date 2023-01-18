#!/usr/bin/env bash

# clone_code(){
#     # Finding free temporary folder
# #    TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-speedy_copy${count}"; }
#     count=1 # Code copy number
#     while [[ -d $(TMP_CODE_DIR) ]]; do (( count++ )); done
# 
#     create_code_copy "$(TMP_CODE_DIR)"
#     cd "$(TMP_CODE_DIR)/code"
#     source ./initialize_das.sh
# }
# 
# #-----------------------------------------------------------------------
# #> Copy code excluding git repository, doxydoc, disabled stuff and docs
# #-----------------------------------------------------------------------
# create_code_copy(){
#     funct_opening 1
# 
#     output_dir=$1
#     find .. \
#         \( -type f -o -type l \) \
#         -a \( ! -path "*bin*" \) \
#         -a \( ! -path "*disabled*" \) \
#         -a \( ! -path "*tmp*" \) \
#         -a \( ! -path "*logs*" \) \
#         -a \( ! -path "*.git*" \) \
#         -a \( ! -path "*doxydoc*" \) \
#         -a \( ! -path "*docs*" \) \
#         | while read file_path; do
#         #echo $file_path 1>&2
#         file_path__=${file_path:3} # removing first 2 characters
#         file_dir="${file_path__%/*}"
#         # echo "file_path  = $file_path"
#         # echo "file_path__= $file_path__"
#         # echo "file_dir   = $file_dir"
#         mkdir -p "$output_dir/$file_dir"
#         cp -raL "$file_path" "$output_dir/$file_path__" # cp preserving attributes
#     done
#     echo " A copy of the code was created in"
#     echo " $output_dir"
# 
#     funct_closing 1
# }

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

# speedy_build(){
#     RES=t30          # Spectral resolution
#     F77=gfortran          # Compiler to be used
# 
#     FFLAGS="-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none -fconvert=big-endian"
#     #FFLAGS="-fdefault-real-8 -O3 -ffree-line-length-none -fconvert=big-endian"
#     DBG_FLAGS=""
# #COMPILE=gfortran-4.4
# #COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -ffree-line-length-none
# ##COMOTT1=-fdefault-real-8 -fno-align-commons -O3 -march=native -ffast-math -funroll-loops
# #COMCONV=-fconvert=big-endian
# #COMLIB1=
# 
#     configure
# 
#     echo " Gathering code in bin folder"
#     rm -rf $BIN_DIR; mkdir -p $BIN_DIR; cd $BIN_DIR
# 
#     echo " - Original model source files"
#     find "$CODE_DIR/speedy_ver32/source" -maxdepth 1 -type f | xargs -I{} cp {} .
#     mv par_horres_${RES}.h atparam.h
#     mv par_verres.h        atparam1.h
# 
#     echo " - Parameter and namelist files"
#     cp $CODE_DIR/speedy_ver32/ver32.input/cls_*.h .
#     #cp $SPEEDY/model/ver32.input/inpfiles.s  $CA/
#     #cp $SPEEDY/model/ver32.input/cls_*.h     $SPEEDY/model/input/exp_$2
#     #cp $SPEEDY/model/ver32.input/inpfiles.s  $SPEEDY/model/input/exp_$2
# 
#     echo " - Modified model files"
#     cp $CODE_DIR/speedy_ver32/update/* .
# 
#     echo " Compiling model"
# 
#     FILES=( \
#         at_gcm.f \
#         dyn_geop.f \
#         dyn_grtend.f \
#         dyn_implic.f \
#         dyn_sptend.f \
#         dyn_step.f \
#         dyn_stloop.f \
#         ini_impint.f \
#         ini_indyns.f \
#         ini_inforc.f \
#         ini_iniall.f \
#         ini_inphys.f \
#         ini_inirdf.f \
#         ini_invars.f \
#         ini_stepone.f \
#         phy_convmf.f \
#         phy_fordate.f \
#         phy_lscond.f \
#         phy_phypar.f \
#         phy_radiat.f \
#         phy_shtorh.f \
#         phy_suflux.f \
#         phy_vdifsc.f \
#         ppo_diagns.f \
#         ppo_restart.f \
#         ppo_setctl.f \
#         ppo_setctl_daily.f \
#         ppo_setgrd.f \
#         ppo_tminc.f \
#         ppo_tminc_daily.f \
#         ppo_tmout.f \
#         ppo_tmout_daily.f \
#         ppo_iogrid.f \
#         sfc_anomod.f \
#         spe_matinv.f \
#         spe_spectral.f \
#         spe_subfft_fftpack.f \
#         ta_vars.f \
#         timeinc_6hr.f \
#         ta_tools.f \
#         ppo_set_ctl_grid.f)
# 
#     for FILE in ${FILES[@]}; do
#         echo " - $FILE"
#         $F77 $FFLAGS $DBG_FLAGS -c $FILE
#     done
# 
#     names=(${FILES[@]%.*})
#     for name in ${names[@]}; do
#         objs=(${objs[@]:-} ${name}.o)
#     done
# 
#     #echo " Linking programs"
#     $F77 $FFLAGS $DBG_FLAGS -o at_gcm.exe ${objs[@]}
# 
# }
# 
# configure(){
#     case "$(hostname)" in
#         tux04|tux21|calc02)
#             tux04_F90="gfortran"
#             F90=$tux04_F90
#             F77=gfortran-4.4
#             case "$F90" in
#                 "gfortran")
#                     F90_OPT="-O3 -fconvert=big-endian";;
#                 "ifort")
#                     F90_OPT="-fast -O3 -convert big_endian";;
#                 *)
#                     error "Unknown compiler $F90"
#                     exit 1;;
#             esac
#             ;;
#         calc01|calc03|calc04)
#             tux04_F90="gfortran"
#             F90=$tux04_F90
#             F77=gfortran
#             case "$F90" in
#                 "gfortran")
#                     F90_OPT="-O3 -fconvert=big-endian";;
#                 "ifort")
#                     F90_OPT="-fast -O3 -convert big_endian";;
#                 *)
#                     error "Unknown compiler $F90"
#                     exit 1;;
#             esac
#             ;;
#         soroban|node???)
#             module load blas/gcc/64/1
#             F90=gfortran
#             F77=gfortran
#             case "$F90" in
#                 "gfortran")
#                     F90_OPT="-O3 -fconvert=big-endian";;
#                 "ifort")
#                     F90_OPT="-fast -O3 -convert big_endian";;
#                 *)
#                     error "Unknown compiler $F90"
#                     exit 1;;
#             esac
#             ;;
#         *)
#             error "unknown machine";;
#     esac
# }

# 
# #------------------------------------------------------
# #> @brief Create new doxygen documentation
# #------------------------------------------------------
#     create_doxygen_document(){
#         funct_opening 3
# 
#         HTML_OUTPUT=$CODE_DIR/doxydoc_files
#         HTML_LINK=$CODE_DIR/doxydoc.html
# 
#         echo " - Removing old documentation"
#         rm -f Doxyfile ${HTML_LINK}
#         rm -fr ${HTML_OUTPUT}
# 
#         echo " - Generating Development Log"
#         echo "/*! @page devellog Development Log" > git_log.txt
# #    git log --pretty=oneline --abbrev-commit >> git_log.txt
#         git log --abbrev-commit  >> git_log.txt
#         echo "*/" >> git_log.txt
# 
#         echo " - Creating new doxygen config file"
#         doxygen -g > /dev/null
# 
#         echo " - Customizing it"
#         set -f # Turning globbing off
#         replace "PROJECT_NAME"          "PROJECT_NAME =  \"DAS\""      Doxyfile
#         replace "INPUT "                "INPUT        =  $CODE_DIR" Doxyfile
#         replace "HTML_OUTPUT"           "HTML_OUTPUT  =  $HTML_OUTPUT" Doxyfile
#         replace "FILE_PATTERNS" "FILE_PATTERNS = *.f *.f90 *.sh *.txt" Doxyfile
#         replace "OPTIMIZE_FOR_FORTRAN"  "OPTIMIZE_FOR_FORTRAN   = YES" Doxyfile
#     # Filter for bash scripts
#         replace "FILTER_SOURCE_FILES"   "FILTER_SOURCE_FILES = YES"    Doxyfile
#         replace "INPUT_FILTER" "INPUT_FILTER = \"sed -e 's|#>|///|' -e 's|#|//|' -e 's|\[ -d|\[ -dir|'\"" Doxyfile
#         replace "RECURSIVE"             "RECURSIVE              = YES" Doxyfile
#         replace "EXCLUDE_PATTERNS" \
#             "EXCLUDE_PATTERNS = */disabled/* */doxydoc_files/*"        Doxyfile
#         replace "SOURCE_BROWSER"        "SOURCE_BROWSER         = YES" Doxyfile
#         replace "INLINE_SOURCES"        "INLINE_SOURCES         = YES" Doxyfile
#         replace "GENERATE_LATEX"        "GENERATE_LATEX         = NO"  Doxyfile
#         replace "EXTRACT_ALL"           "EXTRACT_ALL            = YES" Doxyfile
#         replace "EXTRACT_PRIVATE"       "EXTRACT_PRIVATE        = YES" Doxyfile
#         replace "EXTRACT_STATIC"        "EXTRACT_STATIC         = YES" Doxyfile
#         replace "EXTRACT_LOCAL_METHODS" "EXTRACT_LOCAL_METHODS  = YES" Doxyfile
#         replace "HAVE_DOT"              "HAVE_DOT               = YES" Doxyfile
#         replace "CALL_GRAPH"            "CALL_GRAPH             = YES" Doxyfile
#         replace "CALLER_GRAPH"          "CALLER_GRAPH           = YES" Doxyfile
#         replace "HIDE_UNDOC_RELATIONS"  "HIDE_UNDOC_RELATIONS   =  NO" Doxyfile
#         replace "IMAGE_PATH"            "IMAGE_PATH = \"$CODE_DIR/docs\""  Doxyfile
#         replace "DOT_IMAGE_FORMAT"      "DOT_IMAGE_FORMAT       = jpg" Doxyfile
# #    replace "UML_LOOK  "              "UML_LOOK            = YES"  Doxyfile
#         replace "TEMPLATE_RELATIONS"    "TEMPLATE_RELATIONS     = YES" Doxyfile
#         replace "MAX_DOT_GRAPH_DEPTH"   "MAX_DOT_GRAPH_DEPTH    =   3" Doxyfile
#         replace "DOT_MULTI_TARGETS"     "DOT_MULTI_TARGETS      = YES" Doxyfile
#         replace "STRIP_CODE_COMMENTS"   "STRIP_CODE_COMMENTS    =  NO" Doxyfile
#     # Perhaps Bash inclusion can be done through extension mapping
#     #replace "EXTENSION_MAPPING"     "EXTENSION_MAPPING  = .m=C++"  Doxyfile
# 
#     # replace "ENABLE_PREPROCESSING"  "ENABLE_PREPROCESSING   = YES" Doxyfile
#     # replace "MACRO_EXPANSION"       "MACRO_EXPANSION        = YES" Doxyfile
#     # replace "EXPAND_ONLY_PREDEF"    "EXPAND_ONLY_PREDEF     = YES" Doxyfile
# 
#     #QUIET                  = YES
#     #WARNINGS               = NO
#     #WARN_IF_UNDOCUMENTED   = NO
#     #WARN_IF_DOC_ERROR      = NO
#         set +f # Turning globbing on
# 
#         echo " - Calling Doxygen"
#         doxygen Doxyfile > /dev/null
#     # doxygen -u Doxyfile > /dev/null # update flag (not working)
# 
#         echo " - Creating relative symbolic links"
#         ln -fs doxydoc_files/index.html ${HTML_LINK} # relative link for portability
# 
#         rm -f Doxyfile git_log.txt # Cleaning up
# 
#         funct_closing 3
#     }
