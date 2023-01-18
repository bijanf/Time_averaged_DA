#!/usr/bin/env python
import sys
from scipy import *
from subprocess import call
import os
from numpy  import linspace

cycle_length_span = sys.argv[1]
cycle_lengths = eval(cycle_length_span)

cycle_lenghts_file = open("cycle_lengths.dat",'w')
for cycle_length in cycle_lengths:
    cycle_length_str = "%3I"%cycle_length
    cycle_lengths_file.write(cycle_length_str +'\n')

nature_runs.close
