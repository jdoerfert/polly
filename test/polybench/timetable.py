#!/usr/bin/python2
import sys, sets, os

files   = sys.argv[1:]
results = {}

baseline = 'no_opt'

for filename in files:
  #if 'mm' in filename:
    #continue
  fd = open(filename, 'r')
  time = float(fd.read()[:-2])
  fd.close()
  filename = os.path.splitext(os.path.split(filename)[-1])[0]
  if '_' in filename:
    filebaseindex = filename.index('_')
    filebase, fileext = filename[:filebaseindex], filename[filebaseindex+1:]
  else:
    filebase, fileext = filename, baseline
  if filebase not in results:
    results[filebase] = {}
  results[filebase][fileext] = time

key_set = sets.Set()
for filebase, fileresults in results.items():
  key_set.union_update(sets.Set(fileresults.keys()))

print ' %-20s |' % ' ',
keys = []
for key in key_set:
  keys.append(key)
  print ' %-20s |' % key,
print

speedups = {}
for filebase, fileresults in results.items():
  print ' %-20s |' % filebase,
  for key in keys:
    if not key in speedups:
      speedups[key] = (0, 0)
    if key in fileresults:
      res = str(fileresults[key])
      if baseline in fileresults:
        speedup = (fileresults[baseline] / fileresults[key])
        t, v = speedups[key]
        speedups[key] = (t+1, v + speedup)
        resp = '(' + ('%2.2f' % speedup) + ')'
      else:
        resp = '(n/a)'
    else:
      res = 'n/a'
      resp = '(n/a)'
    print ' %-10s%-10s |' % (res, resp),
  print

print '-' * ((len(keys) + 1) * 24)
print ' %-20s |' % ' ',
for key in keys:
  t, v = speedups[key]
  speedup = 'n/a' if t == 0 else v / t
  print ' %-20s |' % speedup ,
print
