#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)

SPEEDY=$CODE_DIR/speedy_ver32
F77=gfortran
PGM=time_increment

target_dir=${1:-"."}

mkdir -p $target_dir
cd $target_dir

rm -f *.o

ln -fs $SPEEDY/update/timeinc_6hr.f .
ln -fs $SPEEDY/update/com_date.h .
ln -fs $CODE_DIR/speedy_tools/${PGM}.f .

$F77 -c timeinc_6hr.f
$F77 -c ${PGM}.f
$F77 -o ${PGM}.exe *.o

rm -f timeinc_6hr.f com_date.h *.o

echo "${PGM}.exe created succesfully"

exit
