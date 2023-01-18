#!/usr/bin/env bash
#------------------------------------------------------
#> @brief Create or update doxygen documentation
#------------------------------------------------------
create_doxygen_doc(){
    echo "===================================="
    echo " Creating Doxygen documentation"
    echo "===================================="
    
    echo " - Initializing paths"
    HERE=$( dirname "${BASH_SOURCE[0]}" )
    cd $HERE
    cd ..; CODE_FOLDER=$(pwd)
    cd ..;  START_PATH=$(pwd)
    cd $HERE
    HTML_OUTPUT=${START_PATH}/code_doxydoc_files
      HTML_LINK=${START_PATH}/code_doxydoc.html
    
    echo " - Setting doxygen config file"
    replace "INPUT="       "INPUT= $CODE_FOLDER"      code.Doxyfile
    replace "HTML_OUTPUT=" "HTML_OUTPUT=$HTML_OUTPUT" code.Doxyfile
    
    echo " - Calling Doxygen"
    doxygen code.Doxyfile >> /dev/null
    
    echo " - Creating relative symbolic links"
    ln -fs ${HTML_OUTPUT}/index.html ${HTML_LINK}
    
    echo " Documentation Successfully created."
    echo "===================================="
}

#------------------------------------------------------
#> @brief Find the line starting with string1 in file
#>       and replace it (the whole line) with string2
#------------------------------------------------------
function replace(){
    [[ $# -eq 3 ]] || error "Usage: replace string1 string2 file"
    sed -i "s/$(echo $1 | sed -e 's/\([[\/.*]\|\]\)/\\&/g').*/$(echo $2 | sed -e 's/[\/&]/\\&/g')/g" "$3"
}

set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

create_doxygen_doc $@
exit $?


