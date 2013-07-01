import os, re, sys, copy, random


#################################### OPTIONS ##################################

pre_option_locals  = None
post_option_locals = []
default_options    = {}
options            = {}

pre_option_locals = locals().keys()

################################ OPTION DEFAULTS ##############################

'TODO'
VERBOSE             = False

RECURSIVE           = False
ABSOLUTE_PATHS      = False

ALL_FILES_AT_ONCE   = False

BASE_DIR            = None

FILTER_INCLUDE      = None

FILTER_EXCLUDE      = None

COMMAND             = None

############################## OPTION DEFAULTS END ############################

post_option_locals = locals().keys();
for local in post_option_locals:
  if local in pre_option_locals:
    continue
  else:
    options[local] = locals()[local]

def oGet(option, silent = False):
  if option in options:
    return options[option]
  if not silent and 'VERBOSE' in options and options['VERBOSE']:
    print >> sys.stderr, '>>\tRequested unknown option %s' % option
  return None

def oContains(option):
  if option in options:
    return True
  return False

default_options = copy.deepcopy(options)

################################## OPTIONS END ################################

################################ HELPER FUNCTIONS #############################

def optionsToString(comments=True):
  string = ''
  for option, value in options.iteritems():
    if option.endswith('_'): continue
    if option.startswith('_'): continue
    string += '%-20s = %-20s\n' % (option, value)
  return string

def writeOptions(path = None):
  if not path:
    path = os.path.join(getBaseDir(), 'MyBench.rc')
  if oGet('VERBOSE'):
    print 'Write options to %s' % path
    if os.path.isfile(path):
      print >> sys.stderr, '>>\tThis will overwrite an existing file!'
  try:
    fd = open(path, 'w')
    fd.write('# Step 0\n')
    fd.write(optionsToString())
    fd.close()
  except Exception,e:
    print >> sys.stderr, '>>\tError while writing the options'
    print >> sys.stderr, e
    sys.exit(1)

def readOptions(path = None, stepNo = 0):
  global options
  last_output_dir = oGet('OUTPUT_DIR', silent=True)

  if stepNo:
    lastStepNoStr = str(stepNo - 1)
    for option, value in options.items():
      if option.startswith('_'): continue
      options['_' + lastStepNoStr + '_' + option] = value
      del options[option]
    for option, value in default_options.iteritems():
      options[option] = value

  options['STEP_NO'] = str(stepNo)

  if not path:
    path = os.path.join(getBaseDir(), 'MyBench.rc')
  if not checkPath(path, silent = True):
    print >> sys.stderr, '>>\tNo option file found (tried %s)' % path
    return False

  if oGet('VERBOSE'):
    print 'Read options from %s' % path

  foundStep = False
  read      = True
  stepre    = re.compile(r'.*STEP (?P<stepNo>\d+).*', re.IGNORECASE)
  regexp    = re.compile('<.*?>')

  try:
    fd = open(path, 'r')
    for line in fd:
      line = line.strip()
      if line.count('//'):
        index = line.index('//')
        line  = line[:index]
      if not line: continue
      match = stepre.match(line)
      if match:
        if int(match.group('stepNo')) == stepNo:
          read = True
          foundStep = True
        else:
          read = False
        continue
      if not read:
        continue
      if not line.count('='):
        print >> sys.stderr, '>>\tOptions file contains invalid line\n\t%s' % line
        continue
      index  = line.index('=')
      option = line[:index].strip()
      line   = line[index+1:].strip()
      if line.count('#'):
        index = line.index('#')
        line  = line[:index]
      for match in regexp.finditer(line):
        match_string = match.group()
        if not oContains(match_string[1:-1]):
          continue
        match_value  = str(oGet(match_string[1:-1]))
        line = line.replace(match_string, match_value)
      try:
        value = eval(line[1:-1])
      except:
        value = eval(line)
      options[option] = value
    fd.close()
  except Exception,e:
    print >> sys.stderr, '>>\tError while reading the options'
    print >> sys.stderr, e
    sys.exit(1)


  options['FILTER_INCLUDE_RE_'] = re.compile(oGet('FILTER_INCLUDE')
                      if oGet('FILTER_INCLUDE') else '.*')
  options['FILTER_EXCLUDE_RE_'] = re.compile(oGet('FILTER_EXCLUDE')
                      if oGet('FILTER_EXCLUDE') else '$^')

  return foundStep

def getCommandStr(command, ffile, stepNo):
  command_str  = command
  filename     = os.path.split(ffile)[-1] if ffile else ''

  replacements = [('<FILE>', ffile if ffile else '', False),
                  ('<FILE_NO_EXT>', os.path.splitext(filename)[0] if filename else '', False),
                  ('<STEP_NO>', str(stepNo), False),
                  ('<LAST_FILE_DIR>', os.path.basename(os.path.split(ffile)[0]) if filename else '', False),
                  ('<RANDOM>', str(random.randint(10000, 99999)), False),
                  ]
  changed = True
  while changed:
    changed = False
    for string, rep_string, is_option in replacements:
      regexp      = re.compile(string)
      replacement = rep_string if not is_option else oGet(rep_string, silent=True)

      if not replacement:
        if regexp.search(command_str):
          print >> sys.stderr, '>>\t'+                                        \
              'Regexp: %s matches command but has no replacement (option %s)' \
              % (string, rep_string)
          return None
        continue

      command_str, n = regexp.subn(replacement, command_str)
      changed = changed or n != 0

  regexp = re.compile('<.*?>')
  for match in regexp.finditer(command_str):
      print >> sys.stderr, '>>\t'+                                        \
          'Regexp: %s is unknown'  % (match.group())
      return None
  return command_str

def getBaseDir():
  if oGet('BASE_DIR'):
    return oGet('BASE_DIR')
  return os.curdir

def checkPath(path, silent = False):
  path = os.path.normpath(path)
  if not os.path.exists(path):
    if silent: return False
    print >> sys.stderr,                                                    \
         '>>\tRequested path: \n\t%s\nWhich does not exist' % path
    sys.exit(1)
  return path

def prepPath(path):
  if oGet('ABSOLUTE_PATHS'):
    return checkPath(os.path.abspath(path))
  return checkPath(path)

def listAndJoin(folder, rec = False):
  checkPath(folder)
  if not os.path.isdir(folder):
    return []
  if not rec and not oGet('RECURSIVE'):
    if oGet('VERBOSE'): print 'Recursion is off -- No listing of %s' % folder
    return []
  return map(lambda entry: os.path.join(folder, entry), os.listdir(folder))

def isExcluded(item):
  excluded = oGet('FILTER_EXCLUDE_RE_').search(item)
  if oGet('VERBOSE') and excluded:
    print 'Filter excluded %s' % item
  return excluded

def isIncluded(item):
  included = oGet('FILTER_INCLUDE_RE_').search(item)
  if oGet('VERBOSE') and not included:
    print 'Filter did not include %s' % item
  return included

def collectFiles():
  files = []
  worklist = listAndJoin(prepPath(getBaseDir()), rec = True)
  while worklist:
    item      = worklist.pop()
    if isExcluded(item):
      continue
    worklist += listAndJoin(item)
    if os.path.isfile(item) and isIncluded(item):
      files.append(item)
  return files

############################## HELPER FUNCTIONS END ###########################

readOptions(stepNo = -1)

stepNo = 0
startStepNo = 0 if not oContains('START_STEP') else int(oGet('START_STEP'))

while readOptions(stepNo = stepNo):
  stepNo += 1
  if stepNo - 1 < startStepNo:
    if oGet('VERBOSE'):
      mid = ('Skip STEP %i' % (stepNo - 1))
      print '\n%s %s %s' % ('-' * ((98 - len(mid)) / 2), mid, '-' * ((99 - len(mid)) / 2))
    continue

  if oGet('VERBOSE'):
    mid = ('Start STEP %i' % (stepNo - 1))
    print '\n%s %s %s' % ('-' * ((98 - len(mid)) / 2), mid, '-' * ((99 - len(mid)) / 2))

  if oGet('VERBOSE'):
    print optionsToString(comments = False)
    print '\n'

  pre_step_command = oGet('PRE_STEP_COMMAND')
  if (pre_step_command):
    os.system(pre_step_command)

  command = oGet('COMMAND')
  if not command:
    print >> sys.stderr, '>> S%i:  No command definied,... skip step' % (stepNo - 1)
    continue

  files = collectFiles()
  no_files = len(files)
  if oGet('VERBOSE'):
    print " Found %i matching files" % no_files
    print
    for ffile in files:
      print ffile

  done, printed = 0, 0
  mid = ('STEP %i' % (stepNo - 1)) if not oContains('STEP_NAME') else oGet('STEP_NAME')
  print '0%' + ' ' * ((94 - len(mid)) / 2) + mid + ' ' * ((95 - len(mid)) / 2)+ '100%'

  if oGet('ALL_FILES_AT_ONCE'):
    command_str = getCommandStr(command, None, stepNo - 1)
    files_str = ' '.join(files)
    os.system(command_str + ' ' + files_str)
    print '#' * 100
  else:
    pre_file_command = oGet('PRE_FILE_COMMAND')
    for ffile in files:
      if (pre_file_command):
        pre_file_command_str = getCommandStr(pre_file_command, ffile, stepNo - 1)
        os.system(pre_file_command_str)

      command_str = getCommandStr(command, ffile, stepNo - 1)
      if not command_str:
        print >> sys.stderr, '>> S%i:  Command could not be instanciated,... skip step' % (stepNo - 1)
        break
      if oGet('VERBOSE'):
        print command_str
      os.system(command_str)

      done += 1
      to_print = (done * 100) / no_files
      if (to_print > printed):
        sys.stdout.write("#" * (to_print - printed))
        sys.stdout.flush()
        printed = to_print
    if no_files == 0:
      print '#' * 100
  print

  if oGet('VERBOSE'):
    mid = ('Done STEP %i' % (stepNo - 1))
    print '\n%s %s %s' % ('-' * ((98 - len(mid)) / 2), mid, '-' * ((99 - len(mid)) / 2))

if stepNo == 0:
  writeOptions()
