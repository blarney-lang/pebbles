#!/usr/bin/python3

import re
import sys

if len(sys.argv) != 2:
  print(sys.argv[0], "incorrect usage")
  sys.exit()

f = open(sys.argv[1], "r")
output = ["-- Modified by rmdsp\n"]
types = {}
decls = []
count = 0
in_mac_inst = False
for line in f.readlines():
  if line.strip() == "-- Modified by rmdsp":
    sys.exit()
  m = re.search("signal (.*) : (.*);", line)
  if m:
    types[m.group(1)] = m.group(2)
  if not in_mac_inst and re.search(": fourteennm_mac", line):
    in_mac_inst = True
    signed_x = False
  if in_mac_inst:
    output.append("-- " + line)
    m = re.search(" ax => (.+),", line)
    if m: ax = m.group(1)
    m = re.search(" ay => (.+),", line)
    if m: ay = m.group(1)
    m = re.search(" resulta => (.+)", line)
    if m: resulta = m.group(1)
    if re.search(" signed_max => \"true\"", line):
      signed_x = True
  else:
    output.append(line)
  if in_mac_inst and re.search("\);", line):
    in_mac_inst = False
    count = count+1
    reg1 = resulta + "_1";
    reg2 = resulta + "_2";
    decls.append("signal " + reg1 + " : " + types[resulta] + ";\n")
    decls.append("signal " + reg2 + " : " + types[resulta] + ";\n")
    output.append("mul_" + str(count) + " : PROCESS (clk)\n")
    output.append("BEGIN\n")
    output.append("IF (clk'EVENT AND clk = '1') THEN\n")
    if signed_x:
      product = ("resize("
                   + "signed(" + ax + "(" + ax + "'high) & signed(" + ax + "))"
                   + " * signed(\"0\" & " + ay + "), "
                   + resulta + "'length)")
    else:
      product = "unsigned(" + ax + ") * unsigned(" + ay + ")"
    output.append("  " + reg1 + " <= std_logic_vector(" + product + ");\n")
    #output.append("  " + reg2 + " <= " + reg1 + ";\n")
    #output.append("  " + resulta + " <= " + reg2 + ";\n")
    output.append("  " + resulta + " <= " + reg1 + ";\n")
    output.append("END IF;\n")
    output.append("END PROCESS;\n")
f.close()

output2 = []
saw_begin = False
for line in output:
  if not saw_begin and line.strip() == "begin":
    saw_begin = True
    output2.extend(decls)
  output2.append(line)

f = open(sys.argv[1], "w")
for line in output2:
  f.write(line)
f.close()
