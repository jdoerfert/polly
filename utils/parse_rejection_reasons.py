import sys
import random

if len(sys.argv) < 2:
    sys.exit(1)

suites = {'SPEC/CINT2000': [], 'SPEC/CFP2000': [], 'SPEC/CINT2006': [], 'SPEC/CFP2006': [], 'SPEC/CINT2017': [], 'SPEC/CFP2017': [], 'llvm-ts': []}
stats = {}
seses = {}
all_seses = []
expansion_map = {}
expansion_map_rev = {}

def getName(line):
    name = line[line.index(' in ') + 4:]
    return name.strip()
def getInvName(line):
    # print(line)
    assert ' for ' in line
    name = line[line.index(' for ') + 5:]
    return name.strip()

class SESE:
    def __init__(self, name, folder):
        self.name = name
        self.folder = folder
        self.valid = True
        self.reasons = []
        seses[name] = self
        self.expanded = 0
        all_seses.append(self)

        found = False
        for suite, l in suites.items():
            if suite in folder:
                l.append(self)
                found = True
                break
        if not found:
          suites['llvm-ts'].append(self)

    def __str__(self):
        return 'SESE: %s [%s][#RR: %i]' % (self.name, str(self.valid), len(self.reasons))

    def addLine(self, line):
        if 'invalidate: ' in line:
            self.reasons.append(line[len('invalidate: '):line.index(' for ')].strip())
            self.valid = False
            return

        print(line)
        assert False and "UNKNOWN LINE!"

folder = None
read_non_empty = False
with open(sys.argv[1], 'r') as fd:
    for line in fd:
        if line.startswith('cd /') and 'sandbox' in line and '&&' in line:
            folder = line[3:line[3:].index(' ') + 3][len('/home/buildslave/Polly_corr/sandbox/test-2018-05-03_21-59-23/'):]
            continue
        if not line.startswith('SD: '):
            continue
        line = line[4:].strip()
        # print(line)
        if 'findScops in ' in line:
            SESE(getName(line), folder)
            continue

        if 'findScops' in line:
            continue
        if 'expanded' in line:
            all_seses[-1].expanded += 1
            continue
        # try expanding for.body => for.cond.cleanup146 to for.body => for.cond
        if 'try expanding' in line:
            idx = line.index(' to ')
            old = line[len('try expanding '):idx].strip()
            new = line[idx+4:].strip()
            # print('"%s"'%old)
            # print('"%s"'%new)
            assert old in seses
            old_sese = seses[old]
            # assert old_sese not in expansion_map
            if old_sese in expansion_map:
                old_sese.expanded += 1
            expansion_map[old_sese] = new
            expansion_map_rev[new] = old_sese
            continue

        name = getInvName(line)
        assert name in seses
        sese = seses[name]
        sese.addLine(line)


unified_suites = {}
for suite, l in suites.items():
    found = False
    for year in ['2000', '2006', '2017']:
        if year in suite:
            if 'SPEC' + year in unified_suites:
                unified_suites['SPEC' + year] += l
            else:
                unified_suites['SPEC' + year] = l
            found = True
            break
    if not found:
        unified_suites[suite] = l

print('suites:')
for suite, l in unified_suites.items():
    print(suite, len(l))

max_reasons_size = 0
reasons_sizes = {}
reasons = {}
reasons_combi = {}
unique_reasons = {}

num_valid = 0
num_invalid = 0
for sese in all_seses:
    rs = set(sese.reasons)
    if 'no loops' in rs:
        assert len(rs) == 2
        if 'Unprofitable' in rs:
            rs.remove('Unprofitable')
        if 'UnprofitableNoLoops' in rs:
            rs.remove('UnprofitableNoLoops')
        if 'UnprofitableOnlyLoadsOrStores' in rs:
            rs.remove('UnprofitableOnlyLoadsOrStores')
        assert len(rs) == 1

    max_reasons_size = max(max_reasons_size, len(rs))
    if len(rs) in reasons_sizes:
        reasons_sizes[len(rs)] += 1
    else:
        reasons_sizes[len(rs)] = 1

    if 'Unprofitable' in rs or 'UnprofitableNoLoops' in rs or 'UnprofitableOnlyLoadsOrStores' in rs:
        if len(rs) != 1:
            print(str(sese))
            print(sese.reasons)
            print(rs)
        assert len(rs) == 1

    rss = reasons
    if len(rs) == 1:
        rss = unique_reasons
    else:
        rsl = list(rs)
        rsl.sort()
        rsl = ', '.join(rsl)
        if rsl in reasons_combi:
            reasons_combi[rsl] += 1
        else:
            reasons_combi[rsl] = 1

    for r in rs:
        if r in rss:
            rss[r] += 1
        else:
            rss[r] = 1

    if sese.valid:
        if sese.reasons:
            print(sese, sese.reasons)
        assert not sese.reasons
        num_valid += 1
    else:
        if not sese.reasons:
            print(sese, sese.reasons)
        assert sese.reasons
        num_invalid += 1

print("\n#SESE regions: %i [V: %i][I: %i]" % (len(all_seses), num_valid, num_invalid))
for k, v in reasons.items():
    print('%30s = %7i' % (k, v))

unique_reasons_occ = 0
for k, v in unique_reasons.items():
    unique_reasons_occ += v

unique_reasons_content_str = ''
print('\nUNIQUE reasons [%i][Occ: %i]\n' % (len(unique_reasons), unique_reasons_occ))
i = 0
for k, v in unique_reasons.items():
    print('%30s = %7i' % (k, v))
misc = 0
for k, v in unique_reasons.items():
    if v * 100.0 / num_invalid < 1.0:
        misc += v
        continue
    print('\\node[c%i, legend box]{}; & %s ($%i$) \\\\' % (i, k, v))
    unique_reasons_content_str += '%2.4f//c%i, ' % (v*100.0 / num_invalid, i)
    i += 1

print(unique_reasons_content_str[:-2])
print(', %2.4f//c%i' % ((num_invalid - unique_reasons_occ) * 100.0 / num_invalid, i))
print('\\node[c%i, legend box]{}; & %s ($%i$) \\\\' % (i, 'multiple', (num_invalid - unique_reasons_occ)))
# print(', %2.4f/($%i$)/c%i' % ((num_invalid - unique_reasons_occ) * 100.0 / num_invalid, (num_invalid - unique_reasons_occ), i))
i += 1

if misc:
    print(', %2.4f//c%i' % (misc * 100.0 / num_invalid, i))
    print('\\node[c%i, legend box]{}; & %s ($%i$) \\\\' % (i, 'misc', misc))
    i += 1

print('\nReasons size:\n')
reasons_hist_content_str = ''
for i in range(1 ,max_reasons_size):
    print(' %3i = %7i' % (i, reasons_sizes[i]))
    reasons_hist_content_str += ' (%i,%i)' % (i, reasons_sizes[i])
reasons_hist_content_str += ' (%i,%i)' % (i, reasons_sizes[max_reasons_size - 1])

print(reasons_hist_content_str)

colormap = {}
def getColForR(r):
    if r not in colormap:
        colormap[r] = 'col%i' % (len(colormap) + 1)
    return colormap[r]

def getReasonsNo(r):
    return int(getColForR(r)[3:])


print('\n\ngeneral:')

all = 0
reasons_combi_l = []
for k, v in reasons_combi.items():
    reasons_combi_l.append((v, k))
    if ',' in k:
        all += v

reasons_combi_l.sort(reverse=True)
five_pc = all * 0.05
misc = 0
for (v, k) in reasons_combi_l:
    if ',' not in k:
        continue
    if v < five_pc:
        misc += v

print('\n\nall: ', all)
p = misc * 100.0 / all
no = getReasonsNo('misc')
print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, misc, no, 'misc'))
for (v, k) in reasons_combi_l:
    if ',' not in k or v < five_pc:
        continue
    p = v * 100.0 / all
    no = getReasonsNo(k)
    print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, v, no, k))

ccmap = {}

print('suites:')
for suite, l in unified_suites.items():
    print(suite, len(l))

    max_reasons_size = 0
    reasons_sizes = {}
    reasons = {}
    for k in reasons_combi.keys():
        reasons_combi[k] = 0
    unique_reasons = {}

    num_expanded = 0
    num_valid = 0
    num_invalid = 0
    for sese in l:
        num_expanded += sese.expanded
        rs = set(sese.reasons)
        if 'no loops' in rs:
            assert len(rs) == 2
            if 'Unprofitable' in rs:
                rs.remove('Unprofitable')
            if 'UnprofitableNoLoops' in rs:
                rs.remove('UnprofitableNoLoops')
            if 'UnprofitableOnlyLoadsOrStores' in rs:
                rs.remove('UnprofitableOnlyLoadsOrStores')
            assert len(rs) == 1

        max_reasons_size = max(max_reasons_size, len(rs))
        if len(rs) in reasons_sizes:
            reasons_sizes[len(rs)] += 1
        else:
            reasons_sizes[len(rs)] = 1

        if 'top level' in rs or 'Unprofitable' in rs or 'UnprofitableNoLoops' in rs or 'UnprofitableOnlyLoadsOrStores' in rs:
            if len(rs) != 1:
                print(str(sese))
                print(sese.reasons)
                print(rs)
            assert len(rs) == 1

        rss = reasons
        if len(rs) == 1:
            rss = unique_reasons

        rsl = list(rs)
        rsl.sort()
        rsl = ', '.join(rsl)
        if rsl in reasons_combi:
            reasons_combi[rsl] += 1
        else:
            reasons_combi[rsl] = 1

        for r in rs:
            if r in rss:
                rss[r] += 1
            else:
                rss[r] = 1

        if sese.valid:
            if sese.reasons:
                print(sese, sese.reasons)
            assert not sese.reasons
            num_valid += 1
        else:
            if not sese.reasons:
                print(sese, sese.reasons)
            assert sese.reasons
            num_invalid += 1

    print("\n#SESE regions: %i [V: %i][I: %i][E: %i]" % (len(l), num_valid, num_invalid, num_expanded))
    for k, v in reasons.items():
        print('%30s = %7i' % (k, v))

    unique_reasons_occ = 0
    for k, v in unique_reasons.items():
        unique_reasons_occ += v

    unique_reasons_content_str = ''
    print('\nUNIQUE reasons [%i][Occ: %i]\n' % (len(unique_reasons), unique_reasons_occ))
    for k, v in unique_reasons.items():
        print('%30s = %7i' % (k, v))
    misc = 0
    for k, v in unique_reasons.items():
        if v * 100.0 / num_invalid < 1.0:
            misc += v

    for k, v in unique_reasons.items():
        if v * 100.0 / num_invalid < 1.0:
            continue
        no = getReasonsNo(k)
        print('\\node[c%i, legend box]{}; & %s \\\\' % (no, k))
        p = v*100.0 / num_invalid
        unique_reasons_content_str += '%.4f/%.1f\\%% ($%i$)/c%i, %% %s\n' % (p, p, v, no, k)

    no = getReasonsNo('multiple')
    v = (num_invalid - unique_reasons_occ)
    p = v * 100.0 / num_invalid
    print('\\node[c%i, legend box]{}; & %s \\\\' % (no, 'multiple'))

    if misc:
        no = getReasonsNo('misc')
        print('\\node[c%i, legend box]{}; & %s \\\\' % (no, 'misc'))

    no = getReasonsNo('multiple')
    print(unique_reasons_content_str)
    print('%.4f/%.1f\\%% ($%i$)/c%i, %% %s' % (p, p, v, no, 'multiple'))
    if misc:
        no = getReasonsNo('misc')
        p = misc * 100.0 / num_invalid
        print('%.4f/%.1f\\%% ($%i$)/c%i%% %s' % (p, p, misc, no, 'misc'))

    all = 0
    reasons_combi_l = []
    for k, v in reasons_combi.items():
        reasons_combi_l.append((v, k))
        if ',' in k:
            all += v

    two_pc = all * 0.02
    five_pc = all * 0.05
    reasons_combi_l.sort(reverse=True)
    misc2 = 0
    misc5 = 0
    misc2_n = 0
    misc5_n = 0
    for (v, k) in reasons_combi_l:
        if ',' not in k:
            continue
        if v < two_pc:
            misc2 += v
            misc2_n += 1
        elif v < five_pc:
            misc5 += v
            misc5_n += 1

    print('\n\nall: ', all, ' 2%', two_pc, '(%i)' % misc2_n, ' 5%', five_pc, '(%i)' % misc5_n)
    p = misc2 * 100.0 / all
    no = getReasonsNo('misc')
    print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, misc2, no, 'misc2'))
    p = misc5 * 100.0 / all
    no = getReasonsNo('multiple')
    print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, misc5, no, 'misc5'))
    for (v, k) in reasons_combi_l:
        if ',' not in k or v < five_pc:
            continue
        p = v * 100.0 / all
        no = getReasonsNo(k)
        print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, v, no, k))

    p = misc2 * 100.0 / all
    no = getReasonsNo('misc')
    print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, 'others (<2\\% each)'))
    p = misc5 * 100.0 / all
    no = getReasonsNo('multiple')
    print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, 'others (>2\\% \\& <5\\% each)'))
    for (v, k) in reasons_combi_l:
        if ',' not in k or v < five_pc:
            continue

        abrv = ''
        for l in k:
            if l is ',':
                abrv += ' \\& '
            if not l.isupper():
                continue
            abrv += l
        p = v * 100.0 / all
        no = getReasonsNo(k)
        print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, abrv))


    print('\nReasons size:\n')
    reasons_hist_content_strs = []
    def addrhcs(s, idx):
        if len(reasons_hist_content_strs) <= idx:
            reasons_hist_content_strs.append([])
            return addrhcs(s, idx)
        reasons_hist_content_strs[idx].append(s)

    max_rsi = 0
    for i in range(2 ,max_reasons_size):
        max_rsi = max(max_rsi, reasons_sizes[i])
    for i in range(2 ,max_reasons_size):
        rsi = reasons_sizes[i]

        irsls = []
        for rsl, v in reasons_combi.items():
            if rsl.count(',') != i - 1 or v < 1000:
                continue
            irsls.append((v, rsl))
        irsls.sort(reverse=True)

        cur = 0
        for k in range(len(irsls)):
            v, s = irsls[k]
            idx = k + 1 #int(c[3:])
            cur += v
            if s in colormap:
                c = colormap[s]
                addrhcs('\\addplot[fill=%s] coordinates { (%i,%i) };' % (c, i, cur), idx)
            else:
                if s in ccmap:
                    c0, c1, c2 = ccmap[s]
                else:
                    c0, c1, c2 = random.randint(0,255), random.randint(0,255), random.randint(0,255)
                    ccmap[s] = (c0, c1, c2)
                cs = c0 + c1 + c2
                addrhcs('\\definecolor{cc%i}{RGB}{%i,%i,%i}\\addplot[fill=cc%i] coordinates { (%i,%i) };' % (cs,c0,c1,c2,cs, i, cur), idx)

        addrhcs(' (%i,%i) ' % (i, rsi), 0)
    addrhcs('};', 0)

    # addrhcs(' (%i,%i)};' % (i, reasons_sizes[max_reasons_size - 1]), 0)
    print(' '.join(reasons_hist_content_strs[0]))
    reasons_hist_content_strs = reasons_hist_content_strs[1:]
    reasons_hist_content_strs.reverse()
    for rhcs in reasons_hist_content_strs:
        print('\n'.join(rhcs))



no = getReasonsNo('misc')
print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, 'others (<2\\% each)'))
no = getReasonsNo('multiple')
print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, 'others (>2\\% \\& <5\\% each)'))
for r, no in colormap.items():
    if r == 'misc' or r == 'multiple':
        continue
    no = int(no[3:])
    if ',' not in r:
        print(r)
    assert ',' in r
    abrv = ''
    for l in r:
        if l is ',':
            abrv += ' \\& '
        if not l.isupper():
            continue
        abrv += l
    print('\\node[c%i, legend box]{}; & %10s \\\\' % (no, abrv))
