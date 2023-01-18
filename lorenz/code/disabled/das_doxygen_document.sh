#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create or update doxygen documentation
#------------------------------------------------------
das_doxygen_document(){
    funct_opening 3

    echo " - Initializing paths"
    HERE=$( dirname "${BASH_SOURCE[0]}" ); cd $HERE
    CODE_FOLDER=$(pwd)
    HTML_OUTPUT=${CODE_FOLDER}/doxydoc_files
    HTML_LINK=${CODE_FOLDER}/doxydoc.html

    echo " - Removing old documentation"
    rm -f Doxyfile ${HTML_LINK}
    rm -fr ${HTML_OUTPUT}

    echo " - Generating Development Log"
    echo "/*! @page devellog Development Log" > git_log.txt
#    git log --pretty=oneline --abbrev-commit >> git_log.txt
    git log --abbrev-commit  >> git_log.txt
    echo "*/" >> git_log.txt
    
    echo " - Creating new doxygen config file"
    doxygen -g > /dev/null

    echo " - Customizing it"
    set -f # Turning globbing off
    replace "PROJECT_NAME"          "PROJECT_NAME =  \"DAS\""      Doxyfile
    replace "INPUT "                "INPUT        =  $CODE_FOLDER" Doxyfile
    replace "HTML_OUTPUT"           "HTML_OUTPUT  =  $HTML_OUTPUT" Doxyfile
    replace "FILE_PATTERNS" "FILE_PATTERNS = *.f *.f90 *.sh *.txt" Doxyfile
    replace "OPTIMIZE_FOR_FORTRAN"  "OPTIMIZE_FOR_FORTRAN   = YES" Doxyfile
    # Filter for bash scripts
    replace "FILTER_SOURCE_FILES"   "FILTER_SOURCE_FILES = YES"    Doxyfile
    replace "INPUT_FILTER" "INPUT_FILTER = \"sed -e 's|#>|///|' -e 's|#|//|' -e 's|\[ -d|\[ -dir|'\"" Doxyfile
    replace "RECURSIVE"             "RECURSIVE              = YES" Doxyfile
    replace "EXCLUDE_PATTERNS" \
        "EXCLUDE_PATTERNS = */disabled/* */doxydoc_files/*"        Doxyfile
    replace "SOURCE_BROWSER"        "SOURCE_BROWSER         = YES" Doxyfile
    replace "INLINE_SOURCES"        "INLINE_SOURCES         = YES" Doxyfile
    replace "GENERATE_LATEX"        "GENERATE_LATEX         = NO"  Doxyfile
    replace "EXTRACT_ALL"           "EXTRACT_ALL            = YES" Doxyfile
    replace "EXTRACT_PRIVATE"       "EXTRACT_PRIVATE        = YES" Doxyfile
    replace "EXTRACT_STATIC"        "EXTRACT_STATIC         = YES" Doxyfile
    replace "EXTRACT_LOCAL_METHODS" "EXTRACT_LOCAL_METHODS  = YES" Doxyfile
    replace "HAVE_DOT"              "HAVE_DOT               = YES" Doxyfile
    replace "CALL_GRAPH"            "CALL_GRAPH             = YES" Doxyfile
    replace "CALLER_GRAPH"          "CALLER_GRAPH           = YES" Doxyfile
    replace "HIDE_UNDOC_RELATIONS"  "HIDE_UNDOC_RELATIONS   =  NO" Doxyfile
    replace "IMAGE_PATH"            "IMAGE_PATH = \"$HERE/docs\""  Doxyfile
    replace "DOT_IMAGE_FORMAT"      "DOT_IMAGE_FORMAT       = jpg" Doxyfile
#    replace "UML_LOOK  "              "UML_LOOK            = YES"  Doxyfile
    replace "TEMPLATE_RELATIONS"    "TEMPLATE_RELATIONS     = YES" Doxyfile
    replace "MAX_DOT_GRAPH_DEPTH"   "MAX_DOT_GRAPH_DEPTH    =   3" Doxyfile
    replace "DOT_MULTI_TARGETS"     "DOT_MULTI_TARGETS      = YES" Doxyfile
    replace "STRIP_CODE_COMMENTS"   "STRIP_CODE_COMMENTS    =  NO" Doxyfile
    # Perhaps Bash inclusion can be done through extension mapping
    #replace "EXTENSION_MAPPING"     "EXTENSION_MAPPING  = .m=C++"  Doxyfile

    # replace "ENABLE_PREPROCESSING"  "ENABLE_PREPROCESSING   = YES" Doxyfile
    # replace "MACRO_EXPANSION"       "MACRO_EXPANSION        = YES" Doxyfile
    # replace "EXPAND_ONLY_PREDEF"    "EXPAND_ONLY_PREDEF     = YES" Doxyfile
    
    #QUIET                  = YES
    #WARNINGS               = NO
    #WARN_IF_UNDOCUMENTED   = NO
    #WARN_IF_DOC_ERROR      = NO
    set +f # Turning globbing on

    echo " - Calling Doxygen"
    doxygen Doxyfile > /dev/null
    # doxygen -u Doxyfile > /dev/null # update flag (not working)

    echo " - Creating relative symbolic links"
    ln -fs doxydoc_files/index.html ${HTML_LINK} # relative link for portability

    rm -f Doxyfile git_log.txt # Cleaning up

    funct_closing 3
}
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error
source das_initialize.sh
source ${COM_DAS_DIR}/common_das_tools.sh

das_doxygen_document $@
exit $?
