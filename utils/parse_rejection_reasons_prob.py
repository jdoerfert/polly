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

pairs = [('Unstructured (Exits)', 'NonAffBranch')]
for pair in pairs:
    values = [0,0,0,0]
    for sese in all_seses:
        idx = 0
        if pair[0] in sese.reasons:
            idx += 2
        if pair[1] in sese.reasons:
            idx += 1
        values[idx] += 1
    print('!%s & !%s: %i' % (pair[0], pair[1], values[0]))
    print('!%s &  %s: %i' % (pair[0], pair[1], values[1]))
    print(' %s & !%s: %i' % (pair[0], pair[1], values[2]))
    print(' %s &  %s: %i' % (pair[0], pair[1], values[3]))
