#!/usr/bin/python2
import sys, sets, os

files   = sys.argv[1:]
try:
  maxtimes = int(files[0])
  files = files[1:]
except:
  maxtimes = 0
results = {}

baseline = 'no_opt'

for filename in files:
  #if 'mm' in filename:
    #continue
  fd = open(filename, 'r')
  times = 0
  time  = 0
  try:
    for line in fd:
      time += float(line[:-2])
      times += 1
      if maxtimes and times >= maxtimes:
        break
  except:
    continue
  finally:
    fd.close()
  if times == 0:
    continue
  time /= times
  filename = os.path.splitext(os.path.split(filename)[-1])[0]
  if '_' in filename:
    filebaseindex = filename.index('_')
    filebase, fileext = filename[:filebaseindex], filename[filebaseindex+1:]
  else:
    filebase, fileext = filename, baseline
  fileext  = fileext.replace('only','o')
  if fileext.startswith('polly_'):
    fileext = fileext[6:]
  if filebase not in results:
    results[filebase] = {}
  results[filebase][fileext] = time

key_set = sets.Set()
for filebase, fileresults in results.items():
  key_set.union_update(sets.Set(fileresults.keys()))

print ' %-13s |' % ' ',
keys = []
for key in key_set:
  keys.append(key)
keys.sort()
for key in keys:
  print ' %-13s |' % (key if len(key) < 14 else key[:13]),
print
print '-' * ((len(keys) + 1) * 17 - 1)

speedups = {}
for filebase, fileresults in results.items():
  print ' %-13s |' % (filebase if len(filebase) < 14 else filebase[:13]),
  for key in keys:
    if not key in speedups:
      speedups[key] = (0, 0)
    if key in fileresults:
      res = '%2.4f' % (fileresults[key])
      if baseline in fileresults:
        speedup = (fileresults[baseline] / fileresults[key])
        t, v = speedups[key]
        speedups[key] = (t+1, v + speedup)
        resp = '(' + ('' if speedup > 10 else '0') + ('%2.2f' % speedup) + ')'
      else:
        resp = '(n/a)'
    else:
      res = 'n/a'
      resp = '(n/a)'
    print ' %6s%-7s |' % (res, resp),
  print

print '-' * ((len(keys) + 1) * 17 - 1)
print ' %-13s |' % ' ',
for key in keys:
  t, v = speedups[key]
  speedup = 'n/a' if t == 0 else v / t
  print ' %-13s |' % (str(speedup) if len(str(speedup)) < 14 else str(speedup)[:13]),
print
