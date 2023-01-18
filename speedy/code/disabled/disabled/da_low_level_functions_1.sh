#!/bin/bash

#set_cls_instep(){
    ## time stepping parameters' file
## usage!!!
##      local NMONTS=3
##      local NDAYSL=30
## examples
##    set_cls_instep 12 30 # config for a run lenght of 11 months and 30 days
##    set_cls_instep 1 30  # config for a run lenght of  0 months and 30 days
##    set_cls_instep 1 0 # time config for 6-hr run


      #local NMONTS=$1
      #local NDAYSL=$2
	
      #cat > ${SPEEDY}/model/update/cls_instep.h<<EOF
#C--
#C--   Length of the integration and time stepping constants (common ISTEPS)

      #NMONTS = $NMONTS
      #NDAYSL = $NDAYSL
      #NSTEPS = 36

      #NSTDIA = 36*30
      #NSTPPR = 6
      #NSTOUT = 36*30
      #IDOUT  = 1
      #NMONRS = 3

      #ISEASC = 1
      #IYEAR0 = 1981
      #IMONT0 = 1

      #NSTRAD = 3
      #NSTRDF = 0
      #INDRDF = 0

      #IALST  = 0
      #IASST  = 0
      #IAICE  = 0

      #ISST0  = 25

#C--
#C--   Logical flags (common LFLAG1)

      #LPPRES = .true.
#EOF
#}


timeinc6hr(){
    
    if test $# -ne 4
    then
      echo "USAGE: $0 yyyy mm dd hh"
      return 1
    fi
        
    local YYYY=$1
    local MM=$2
    local DD=$3
    local HH=$4

    local ITMP

    # Increment date
    HH=`expr $HH + 6`
    if test $HH -lt 10
    then
      HH=0$HH
    elif test $HH -gt 23
    then
      HH=00
      DD=`expr $DD + 1`
      if test $DD -lt 10
      then
        DD=0$DD
      elif test $DD -eq 29
      then
        ITMP=`expr $YYYY % 4` || test 1 -eq 1
        if test $MM -eq 02 -a $ITMP -ne 0
        then
          DD=01
          MM=03
        fi
      elif test $DD -eq 30
      then
        ITMP=`expr $YYYY % 4` || test 1 -eq 1
        if test $MM -eq 02 -a $ITMP -eq 0
        then
          DD=01
          MM=03
        fi
      elif test $DD -eq 31
      then
        if test $MM -eq 04 -o $MM -eq 06
        then
          DD=01
          MM=`expr $MM + 1`
          MM=0$MM
        elif test $MM -eq 09 -o $MM -eq 11
        then
          DD=01
          MM=`expr $MM + 1`
        fi
      elif test $DD -eq 32
      then
        DD=01
        MM=`expr $MM + 1`
        if test $MM -lt 10
        then
          MM=0$MM
        fi
      fi
      if test $MM -gt 12
      then
        MM=01
        YYYY=`expr $YYYY + 1`
      fi
    fi
    #
    # Outputs
    #
    echo $YYYY$MM$DD$HH
    return
    #return 0
}




#advance_time(){
    #echo "${IYYYY}-${IMM}-${IDD}"
    #date1=$(date -d ${IYYYY}-${IMM}-${IDD} +"%s")
    #echo $date1
    #date2=$(date -d @${date1} +"%F")
    #echo $date2

#}

#launcher(){
## ------ Functions list  -------------
#functions[0]="spinup_model"
#functions[1]="create_nature_run"
##functions[2]="assimilation_experiment"
##--------------------------------------

## - Execute functions in the list
#echo "$0 Log file" > "$0.log"
#echo 'Number of tasks = '"${#functions[@]}"

#for FUNCT in "${functions[@]}"
#do
  #echo $'\n''Running function '"${FUNCT}"$'\n'
  #eval "${FUNCT}>>$0.log 2>&1"
  ##echo "${FUNCT}>../script_reports${FUNCT}.txt 2>&1"
##  eval "${FUNCT}>${HERE}/../script_reports/${FUNCT}.log 2>&1"
#done
#}

## - Initialize script verbosity
#if [ "$verbose" -gt 0 ]; then
#    VERBOSE_FLAG='-x'
#else
#    VERBOSE_FLAG=''
#fi


