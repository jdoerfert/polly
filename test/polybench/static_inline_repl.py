#!/usr/bin/python2
import sys

file = sys.argv[1]

fd = open(file, 'r')
content = fd.read().split('\n')
fd.close()

fd = open(file, 'w')
for line in content:
  line = line.replace('inline','')
  fd.write(line + '\n')
fd.close()
