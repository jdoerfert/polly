#!/usr/bin/python2
import sys, sets, os

def aggregateTimes(times):
  if (len(times)):
    return sum(times) / len(times)
  return None

def filterTimes(times):
  if len(times) > 3:
    times.sort()
    times = times[max(len(times)/5, 1):-max(len(times)/5, 1)]
  return aggregateTimes(times)


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
  times = []
  try:
    for line in fd:
      times.append(float(line[:-2]))
      if maxtimes and len(times) >= maxtimes:
        break
  except:
    continue
  finally:
    fd.close()
  filename = os.path.splitext(os.path.split(filename)[-1])[0]
  if '_' in filename:
    filebaseindex = filename.index('_')
    filebase, fileext = filename[:filebaseindex], filename[filebaseindex+1:]
  else:
    filebase, fileext = filename, baseline
  #fileext  = fileext.replace('only','o')
  #if fileext.startswith('polly_'):
    #fileext = fileext[6:]
  if filebase not in results:
    results[filebase] = {}
  time = filterTimes(times)
  if time:
    results[filebase][fileext] = time

key_set = sets.Set()
for filebase, fileresults in results.items():
  key_set.union_update(sets.Set(fileresults.keys()))

def reverse(key):
  l = list(key)
  l.reverse()
  return "".join(l)

print ' %-28s |' % ' ',
keys, keys2 = [], []
for key in key_set:
  key =reverse(key)
  keys2.append(key)
keys2.sort()
for key in keys2:
  keys.append(reverse(key))

for key in keys:
  print ' %-28s |' % (key if len(key) < 29 else key[:28]),
print
print '-' * ((len(keys) + 1) * 20 - 1)

speedups = {}
for filebase, fileresults in results.items():
  print ' %-28s |' % (filebase if len(filebase) < 29 else filebase[:28]),
  for key in keys:
    if not key in speedups:
      speedups[key] = (0, 0)
    if key in fileresults:
      res = '%3.8f' % (fileresults[key])
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
    print ' %18s%-10s |' % (res, resp),
  print

print '-' * ((len(keys) + 1) * 20 - 1)
print ' %-28s |' % ' ',
for key in keys:
  t, v = speedups[key]
  speedup = 'n/a' if t == 0 else v / t
  print ' %-28s |' % (str(speedup) if len(str(speedup)) < 29 else str(speedup)[:28]),
print
