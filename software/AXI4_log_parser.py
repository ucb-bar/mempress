# Verifies correct implementation of AXI4 protocol based on prints from each AXI4 channel
#
# Arguments
#     arg1: absolute path to .out file
#
# Example Usage
#     python3 AXI4_log_parser.py /scratch/edwinlim/hyperscale-soc-chipyard/sims/firesim/deploy/results-workload/2023-01-01--02-38-35-mempress-bare-1stream/mempress-bare-1stream0/metasim_stderr.out

import sys
import re

id_pattern = 

filepath = sys.argv[1]
file = open(filepath, 'r')
lines = file.readlines()

for line in lines:
    if "AR FIRE BEGIN" in line:
      print(line, end="")

file.close()

