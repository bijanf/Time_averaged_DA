create_archive_dir(){
    #---------------------------------
    # Create or replace archive_dir
    #---------------------------------
    local archive_dir=$1

    if [ -d $archive_dir ]; then
        echo "There is already a dataset called $( basename $archive_dir )"
        read -p "Do you want to replace it? y/* " yn
        case $yn in
             [Yy]* ) rm -rf $archive_dir; mkdir -p $archive_dir;;
                 * ) return 1;;
        esac
    else
        mkdir -p $archive_dir
    fi
}

error_exit() {
	#---------------------------------------------------------------
	# Function for exit due to fatal program error
	# Accepts 1 argument:
	#			string containing descriptive error message
	# Requires the variable PROGNAME to be set in the calling script:
	# PROGNAME=$(basename $0)
	#----------------------------------------------------------------
	# Example call of the error_exit function:
	# (Note the inclusion of the LINENO environment variable.
	#  It contains the current line number).
	
	# echo "Example of error with line number and message"
	# error_exit "$LINENO: An error has occurred."
	#----------------------------------------------------------------
	info=$(caller 0)        # Calling function info
	OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
	info_array=( $info )
	IFS="$OLD_IFS"          # restoring original file separator 

    funct_line=${info_array[0]}
    funct_name=${info_array[1]}
    funct_file=${info_array[2]}
    
	echo "Error in function '${funct_name}' ($(basename $funct_file) line ${funct_line}) : ${1:-"Unknown Error"}" 1>&2
#	echo "${FUNCNAME[ 0 ]} error: ${1:-"Unknown Error"}" 1>&2
	exit 1
}


aqui voy


funct_opening(){
  if [ $# -ne 0 ]; then
    info=$(caller 0)        # Calling function info
	OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
	info_array=( $info )
	IFS="$OLD_IFS"          # restoring original file separator 

#    funct_line=${info_array[0]}
    funct_name=${info_array[1]}
#    funct_file=${info_array[2]}
    echo "============================================"
    echo " ${funct_name} START"
    echo "============================================"
  else
    if [ $verbose -ge 1 ]; then
 
 	  info=$(caller 0)        # Calling function info
	  OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
	  info_array=( $info )
	  IFS="$OLD_IFS"          # restoring original file separator 

#      funct_line=${info_array[0]}
      funct_name=${info_array[1]}
#      funct_file=${info_array[2]}
      echo "******************************************"
      echo " ${funct_name} START"
      echo "******************************************"
    fi
  fi
}

funct_closing(){
  if [ $# -ne 0 ]; then
	info=$(caller 0)        # Calling function info
	OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
	info_array=( $info )
	IFS="$OLD_IFS"          # restoring original file separator 

#    funct_line=${info_array[0]}
    funct_name=${info_array[1]}
#    funct_file=${info_array[2]}

    echo "============================================"
    echo " ${funct_name} NORMAL END"
    echo "============================================"

  else
    if [ $verbose -ge 1 ]; then
  
	  info=$(caller 0)        # Calling function info
	  OLD_IFS="$IFS"; IFS=" " # setting temporally info separator
	  info_array=( $info )
	  IFS="$OLD_IFS"          # restoring original file separator 

#      funct_line=${info_array[0]}
      funct_name=${info_array[1]}
#      funct_file=${info_array[2]}

      echo "******************************************"
      echo " ${funct_name} NORMAL END"
      echo "******************************************"
    fi
  fi
}

setup_modules_system(){
    MODULE_VERSION=3.2.8
    MODULEPATH=/net/opt/system/modules/$MODULE_VERSION/modulefiles
    
    export MODULEPATH
    
    case "$0" in
              -sh|sh|*/sh)  modules_shell=sh ;;
           -ksh|ksh|*/ksh)  modules_shell=ksh ;;
           -zsh|zsh|*/zsh)  modules_shell=zsh ;;
        -bash|bash|*/bash)  modules_shell=bash ;;
    esac
    
    module() { eval `/net/opt/system/modules/$MODULE_VERSION/bin/modulecmd bash $*`; }
    
    module load modules
}
