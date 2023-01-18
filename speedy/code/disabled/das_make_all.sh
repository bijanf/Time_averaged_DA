#!/usr/bin/env bash
set -o errexit # non-zero return values make the script halt
set -o nounset # Unset variable expansion gives error

echo "============================================"
echo " Compiling das fortran codes"
echo "============================================"
echo " - speedy"
make_speedy.sh              > /dev/null

echo " - create_observations"
# make_create_observations.sh > /dev/null
# make_obsmake.sh             > /dev/null

echo " - time_increment"
make_time_increment.sh      > /dev/null

# echo " - letkf"
# make_letkf.sh               > /dev/null

# echo " - addition"
# make_calculate_addition.sh  > /dev/null

# echo " - difference"
# make_calculate_difference.sh> /dev/null

# echo " - variance"
# make_calculate_variance.sh  > /dev/null

echo " - mean"
make_mean.sh > /dev/null

echo "---------------------------------------------"
echo " All fortran programs compiled successfully"
echo "============================================="
