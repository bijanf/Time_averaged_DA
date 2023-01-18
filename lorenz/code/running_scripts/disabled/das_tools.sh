#!/usr/bin/env bash

#clone_code(){
    ## Finding free temporary folder
    #TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das-lorenz_copy${count}"; }
    #count=1 # Code copy number
    #while [[ -d $(TMP_CODE_DIR) ]]; do (( count++ )); done

    #create_code_copy "$(TMP_CODE_DIR)"
    #cd "$(TMP_CODE_DIR)/code"
    #source ./initialize_das.sh
#}

##-----------------------------------------------------------------------
##> Copy code excluding git repository, doxydoc, disabled stuff and docs
##-----------------------------------------------------------------------
#create_code_copy(){
    #funct_opening 1

    #output_dir=$1
    #find .. \
        #\( -type f -o -type l \) \
        #-a \( ! -path "*bin*" \) \
        #-a \( ! -path "*disabled*" \) \
        #-a \( ! -path "*tmp*" \) \
        #-a \( ! -path "*logs*" \) \
        #-a \( ! -path "*.git*" \) \
        #-a \( ! -path "*doxydoc*" \) \
        #-a \( ! -path "*docs*" \) \
        #-a \( ! -path "*#*" \) \
  #| while read file_path; do
        #file_path__=${file_path:3} # removing first 2 characters
        #file_dir="${file_path__%/*}"
        ## echo "file_path  = $file_path"
        ## echo "file_path__= $file_path__"
        ## echo "file_dir   = $file_dir"
        #mkdir -p "$output_dir/$file_dir"
        #cp -ra "$file_path" "$output_dir/$file_path__" # cp preserving attributes
    #done
    #echo " A copy of the code was created in"
    #echo " $output_dir"

    #funct_closing 1
#}

# create_code_copy(){
#     funct_opening 1

#     output_dir=$1
#     cd $CODE_DIR/..
#     find ./code \
#         ! -wholename '*.git*'     ! -wholename '*doxydoc_files*' \
#         ! -wholename '*disabled*' ! -wholename '*docs*' \
#         ! -wholename '*logs*' | \
#         while read file; do
#         # ! -wholename '*logs*'       -type f | \
#         path=$(echo $file | cut -b 3-) # removing first 2 characters
#         folder="${path%/*}"
#         mkdir -p "$output_dir/$folder"
#         cp -ra "$file" "$output_dir/$path" # cp preserving attributes
#     done
#     echo " A copy of the code was created in"
#     echo " $output_dir"

#     funct_closing 1
# }

#=========================================================================
#> @brief Compile all fortran programs for a given model
#=========================================================================
das_make_all(){
    funct_opening 3

    model_name=$1

    echo " Building programs for model $model_name with $F90"

    configure

    echo " Copying sources to bin folder"

    MODEL_DIR=$CODE_DIR/models/$model_name
    rm -rf $BIN_DIR; mkdir -p $BIN_DIR; cd $BIN_DIR
    cp $COM_DIR/*.{f,f90} .
    cp $MODEL_DIR/model_core.f90 .
#    cat $COM_DIR/netlibblas.f >> ./netlib.f

    echo " Compiling modules"

    modules=( \
        SFMT.f90 \
        common.f90 \
        common_tools.f90 \
        ensemble_size.f90 \
        station_data.f90 \
        model_core.f90 \
        dynamical_system.f90 \
        io_tools.f90 \
        observation_operator.f90 \
        ensemble_tools.f90 \
        trajectory.f90 \
        filter.f90 \
        )

    for module in ${modules[@]}; do
        echo " - $module"
        $F90 $FFLAGS $DBG_FLAGS -c $module
    done

    echo " Linking programs"

    programs=( \
        sampling_run \
        nature_run \
        nature_obs \
        ensemble_free_run \
        ensemble_assi_run \
        )

    names=(${modules[@]%.*})
    for name in ${names[@]}; do
        objs=(${objs[@]:-} ${name}.o)
    done

    for program_name in ${programs[@]}; do
        echo " - $program_name"
        $F90 $FFLAGS $DBG_FLAGS -o ${program_name}.exe ${program_name}.f90 ${objs[@]}
    done

    funct_closing 3
}

#=========================================================================
#> @brief Set machine dependent compiling variables
#=========================================================================
configure(){
    funct_opening 2

    echo " Finding out right configuration"

    # - Identifying server
    echo " - Running in $(hostname)"

    # - Configuring compilers
    case "$F90" in
        gfortran) gfortran_config ;;
        ifort   )    ifort_config ;;
        *       ) error "Unsupported compiler $F90";;
    esac

    funct_closing 2
}

gfortran_config(){

    FFLAGS="-ffree-line-length-none -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow
    # FFLAGS="-ffree-line-length-none -lblas -Wall -Wextra -finit-real=nan -ffpe-trap=invalid,zero,overflow" #,undeflow

    case $DBG in
        "on" )
            DBG_FLAGS="-g3 -fbacktrace -fbounds-check"
            echo " - Compiling with debugging flags" ;;
        "off")
            DBG_FLAGS="-O3"
            echo " - Compiling with optimization"    ;;
        *    )
            error "Unknown debugging status $DBG"   ;;
    esac

    case "$(hostname)" in
        negrito);;
        tux04 |tux21|calc01|calc02|calc03|calc04);;
        soroban|node???) module load blas/gcc/64/1          ;;
        *              ) error " unknown machine $(hostname)";;
    esac
    }

    ifort_config(){

        FFLAGS="-fpe=0 -fpe-all=0 -extend-source -warn"
    #FFLAGS="-lblas -fpe=0 -fpe-all=0 -extend-source -warn"
        case $DBG in
            "on" )
                DBG_FLAGS="-O0 -g -traceback -check"
#           DBG_FLAGS="-O0 -debug all -traceback -check bounds"
                echo " - Compiling with debugging flags" ;;
            "off")
                DBG_FLAGS="-O3"
                echo " - Compiling with optimization"    ;;
            *    )
                error "Unknown debugging status $DBG"    ;;
        esac

        case "$(hostname)" in
            negrito)            ;;
            tux04|calc02|tux21) ;;
            calc01|calc03|calc04)
                source /net/opt/system/modules/default/init/bash
                module load ifort/12.0.2
                ;;
            soroban|node???)
                module load intel/compiler/64/12.0/2011.4.191
                ;;
            * ) error " unknown machine $(hostname)";;
        esac
    }

##------------------------------------------------------
##> @brief Create new doxygen documentation
##------------------------------------------------------
    #create_doxygen_document(){
        #funct_opening 3

        #HTML_OUTPUT=$CODE_DIR/doxydoc_files
        #HTML_LINK=$CODE_DIR/doxydoc.html

        #echo " - Removing old documentation"
        #rm -f Doxyfile ${HTML_LINK}
        #rm -fr ${HTML_OUTPUT}

        #echo " - Generating Development Log"
        #echo "/*! @page devellog Development Log" > git_log.txt
##    git log --pretty=oneline --abbrev-commit >> git_log.txt
        #git log --abbrev-commit  >> git_log.txt
        #echo "*/" >> git_log.txt

        #echo " - Creating new doxygen config file"
        #doxygen -g > /dev/null

        #echo " - Customizing it"
        #set -f # Turning globbing off
        #replace "PROJECT_NAME"          "PROJECT_NAME =  \"DAS\""      Doxyfile
        #replace "INPUT "                "INPUT        =  $CODE_DIR" Doxyfile
        #replace "HTML_OUTPUT"           "HTML_OUTPUT  =  $HTML_OUTPUT" Doxyfile
        #replace "FILE_PATTERNS" "FILE_PATTERNS = *.f *.f90 *.sh *.txt" Doxyfile
        #replace "OPTIMIZE_FOR_FORTRAN"  "OPTIMIZE_FOR_FORTRAN   = YES" Doxyfile
    ## Filter for bash scripts
        #replace "FILTER_SOURCE_FILES"   "FILTER_SOURCE_FILES = YES"    Doxyfile
        #replace "INPUT_FILTER" "INPUT_FILTER = \"sed -e 's|#>|///|' -e 's|#|//|' -e 's|\[ -d|\[ -dir|'\"" Doxyfile
        #replace "RECURSIVE"             "RECURSIVE              = YES" Doxyfile
        #replace "EXCLUDE_PATTERNS" \
            #"EXCLUDE_PATTERNS = */disabled/* */doxydoc_files/*"        Doxyfile
        #replace "SOURCE_BROWSER"        "SOURCE_BROWSER         = YES" Doxyfile
        #replace "INLINE_SOURCES"        "INLINE_SOURCES         = YES" Doxyfile
        #replace "GENERATE_LATEX"        "GENERATE_LATEX         = NO"  Doxyfile
        #replace "EXTRACT_ALL"           "EXTRACT_ALL            = YES" Doxyfile
        #replace "EXTRACT_PRIVATE"       "EXTRACT_PRIVATE        = YES" Doxyfile
        #replace "EXTRACT_STATIC"        "EXTRACT_STATIC         = YES" Doxyfile
        #replace "EXTRACT_LOCAL_METHODS" "EXTRACT_LOCAL_METHODS  = YES" Doxyfile
        #replace "HAVE_DOT"              "HAVE_DOT               = YES" Doxyfile
        #replace "CALL_GRAPH"            "CALL_GRAPH             = YES" Doxyfile
        #replace "CALLER_GRAPH"          "CALLER_GRAPH           = YES" Doxyfile
        #replace "HIDE_UNDOC_RELATIONS"  "HIDE_UNDOC_RELATIONS   =  NO" Doxyfile
        #replace "IMAGE_PATH"            "IMAGE_PATH = \"$HERE/docs\""  Doxyfile
        #replace "DOT_IMAGE_FORMAT"      "DOT_IMAGE_FORMAT       = jpg" Doxyfile
##    replace "UML_LOOK  "              "UML_LOOK            = YES"  Doxyfile
        #replace "TEMPLATE_RELATIONS"    "TEMPLATE_RELATIONS     = YES" Doxyfile
        #replace "MAX_DOT_GRAPH_DEPTH"   "MAX_DOT_GRAPH_DEPTH    =   3" Doxyfile
        #replace "DOT_MULTI_TARGETS"     "DOT_MULTI_TARGETS      = YES" Doxyfile
        #replace "STRIP_CODE_COMMENTS"   "STRIP_CODE_COMMENTS    =  NO" Doxyfile
    ## Perhaps Bash inclusion can be done through extension mapping
    ##replace "EXTENSION_MAPPING"     "EXTENSION_MAPPING  = .m=C++"  Doxyfile

    ## replace "ENABLE_PREPROCESSING"  "ENABLE_PREPROCESSING   = YES" Doxyfile
    ## replace "MACRO_EXPANSION"       "MACRO_EXPANSION        = YES" Doxyfile
    ## replace "EXPAND_ONLY_PREDEF"    "EXPAND_ONLY_PREDEF     = YES" Doxyfile

    ##QUIET                  = YES
    ##WARNINGS               = NO
    ##WARN_IF_UNDOCUMENTED   = NO
    ##WARN_IF_DOC_ERROR      = NO
        #set +f # Turning globbing on

        #echo " - Calling Doxygen"
        #doxygen Doxyfile > /dev/null
    ## doxygen -u Doxyfile > /dev/null # update flag (not working)

        #echo " - Creating relative symbolic links"
        #ln -fs doxydoc_files/index.html ${HTML_LINK} # relative link for portability

        #rm -f Doxyfile git_log.txt # Cleaning up

        #funct_closing 3
    #}
