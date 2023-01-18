time_increment(){
    #------------------------------------------------------------------
    # This function advances "time_one" by the time lenght "increment"
    # and returns the result via standard output. It is assumed a 
    # "normal" calendar (365 day years with leap year every 4 years)
    #-----------------------------------------------------------------
    # Description:
    # Due to heteregeneous month lenght, the first increase round
    # produces oversized months, which can be fixed easily
    
    # time structure examples
    # 0000000006 #  6 hours
    # 0000000100 #  1 day
    # 0000000100 # 10 days
    # 0000003000 # 30 days
    # 0000030000 #  3 months

    # Issues
    # There are still some problematic cases:
    # - month increase when new month days number >= old month days number
    # - year increase starting from Feb 29 of leapyear

    # Solution
    # For the sake of simplicity year and month time increments are
    # only allowed if day increment is zero
    # In general month increase seems to be incompatible with day increases

    #-------------------------------
    # 0 Parsing row input arguments   
    #-------------------------------
    if [ $# -ne 2 ]; then
      echo "USAGE: $0 time_one increment"
      return 1
    fi

    local time_one=$1
    local increment=$2   
    if [ ${#time_one} -ne 10 ]; then
        echo "Error: Wrong time_one string lengt"
        return 1
    fi

    if [ ${#increment} -ne 10 ]; then
        echo "Error: Wrong increment string lengt"
        return 1
    fi
        
    YYYY=`echo $time_one | cut -c1-4`
      MM=`echo $time_one | cut -c5-6`
      DD=`echo $time_one | cut -c7-8`
      HH=`echo $time_one | cut -c9-10`

    INC_YYYY=`echo $increment | cut -c1-4`
      INC_MM=`echo $increment | cut -c5-6`
      INC_DD=`echo $increment | cut -c7-8`
      INC_HH=`echo $increment | cut -c9-10`
  
    #---------------------------------------
    # -1 Finding out present month lenght
    #---------------------------------------

    LEAP_YEAR=`expr $YYYY % 4 == 0` # Condition for leap year
    
    case $MM in
        02                  ) MONTH_LENGHT=`expr 28 + $LEAP_YEAR`;;    
        04|06|09|11         ) MONTH_LENGHT=30;;
        01|03|05|07|08|10|12) MONTH_LENGHT=31;;
#        *) echo "Invalid month number"; return 1;; 
     esac      
    # echo "MONTH_LENGHT: $MONTH_LENGHT"
    
    #--------------------------------
    # 0 Parsing input time arguments   
    #--------------------------------
    if [ $HH -lt 0 ] || [ $HH -gt 23 ]; then
        echo "Error: Present hour $HH is out of bounds"
        return 1
    fi
    if [ $DD -gt $MONTH_LENGHT ]; then
        echo "Error: Present day $DD is out of bounds for month $MM"
        return 1
    fi
    if [ $MM -lt 1 ] || [ $MM -gt 12 ]; then
        echo "Error: Present month $MM is out of bounds"
        return 1
    fi
    if [ $INC_HH -lt 0 ] || [ $INC_HH -gt 23 ]; then
        echo "Error: hour increment $INC_HH is out of bounds. Max value: 23"
        return 1
    fi
    if [ $INC_DD -lt 0 ] || [ $INC_DD -gt 30 ]; then
        echo "Error: Day increment $INC_DD is out of bounds. Max value: 30"
        return 1
    fi
    if [ $INC_MM -ne 0 ] || [ $INC_YYYY -ne 0 ] ; then
        if [ $DD -ne 1 ] ; then
            echo "Error: Forbiden arguments combination:"
            echo "YEAR and MONTH time increments are only allowed if day is equal to 1"
            return 1
        fi
#        if [ $INC_DD -ne 0 ] ; then
        if [ $INC_DD -ne 0 ] || [ $INC_HH -ne 0 ] ; then
            echo "Forbiden arguments combination:"
            echo "YEAR and MONTH time increments are not allowed if DAY and HOUR time increments are 0"
            return 1
        fi
    fi
    
    #-------------------------
    # 1 First increment round    
    #-------------------------
    # - Hour Increment
    HH=`expr $HH + $INC_HH`
    if   [ $HH -ge 24 -a $HH -lt 48 ]; then
          HH=`expr $HH - 24`; INC_DD=`expr $INC_DD + 1`
    elif [ $HH -ge 48 ]; then
          echo "error -> Hour increment >= 24"
          return 1
    fi    

    # - Day Increment
    DD=`expr $DD + $INC_DD`
    if [ $DD -gt $MONTH_LENGHT ]; then
        DD=`expr $DD - $MONTH_LENGHT`; INC_MM=`expr $INC_MM + 1`
    fi    
    
    # Month Increment
    MM=`expr $MM + $INC_MM`
    if [ $MM -gt 12 ]; then
        MM=`expr $MM - 12`;  INC_YYYY=`expr $INC_YYYY + 1`
    fi
    
    # Year Increment
    YYYY=`expr $YYYY + $INC_YYYY`
    
    #---------------------------
    # 2 Fixing oversized months    
    #---------------------------
    LEAP_YEAR=`expr $YYYY % 4 == 0` # Condition for leap year
    
    # - Finding out month lenght
    case $MM in
        2              ) MONTH_LENGHT=`expr 28 + $LEAP_YEAR`;;    
        4|6|9|11       ) MONTH_LENGHT=30;;
        1|3|5|7|8|10|12) MONTH_LENGHT=31;;
        *) echo "Invalid month number"; return 1;; 
     esac
     
    if [ $DD -gt $MONTH_LENGHT ];  then
        DD=`expr $DD - $MONTH_LENGHT`; MM=`expr $MM + 1`
    fi    
    if [ $MM -gt 12 ]; then
        MM=`expr $MM - 12`; YYYY=`expr $YYYY + 1`
    fi
    #------------------------
    # 3 Output updated time    
    #------------------------
    echo `printf '%04d%02d%02d%02d' $YYYY $MM $DD $HH`
    return    
}
