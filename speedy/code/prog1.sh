#!/bin/bash

set -ex 
for i in {2..1000};
do
echo $i
cp q1_3.m q2.m
sed -i "s/1_c.mat/${i}_b.mat/g" q2.m
matlab -nodisplay -nojvm -r 'q2; exit;'
done



