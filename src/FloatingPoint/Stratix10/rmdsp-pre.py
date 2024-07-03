#!/usr/bin/python3

import re
import sys

if len(sys.argv) != 2:
  print(sys.argv[0], "incorrect usage")
  sys.exit()

f = open(sys.argv[1], "r")
output = []
in_hard_fp_block = False
for line in f.readlines():
  if re.search("<ipxact:name>enable_hard_fp</ipxact:name>", line):
    in_hard_fp_block = True
  if in_hard_fp_block and re.search("<ipxact:value>", line):
    output.append("<ipxact:value>false</ipxact:value>\n")
    in_hard_fp_block = False
  else:
    output.append(line)
f.close()

f = open(sys.argv[1], "w")
for line in output:
  f.write(line)
f.close()
