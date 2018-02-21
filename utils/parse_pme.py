import sys

if len(sys.argv) < 2:
    sys.exit(1)

suites = {'SPEC/CINT2000':[], 'SPEC/CFP2000':[], 'SPEC/CINT2006': [], 'SPEC/CFP2006': [], 'SPEC/CINT2017': [], 'SPEC/CFP2017':[], 'llvm-ts': []}
stats = {}
scops = []

def getName(line):
    name = line[line.index(': ') + 2:]
    if '[#AL' in name:
        name = name[:name.index('[#AL')]
    return name.strip()

def getAlBlVpAp(line):
    line = line[line.index('[#AL: ') + 5:]
    al = int(line[:line.index(']')])
    line = line[line.index('[#BL: ') + 5:]
    bl = int(line[:line.index(']')])
    line = line[line.index('[VP: ') + 4:]
    vp = int(line[:line.index(']')])
    line = line[line.index('[AP: ') + 4:]
    ap = int(line[:line.index(']')])
    return (al, bl, vp, ap)

class Scop:
    def __init__(self, name, folder, al, bl, vp, ap):
        self.name = name
        self.folder = folder
        self.als = [al]
        self.bls = [bl]
        self.vps = [vp]
        self.aps = [ap]

        self.asts = []

        self.prune_seen = False
        self.prune_profitable = False
        self.prune_unprofitable = False

        self.ast_seen = False
        self.ast_equal = False
        self.ast_different = False

        self.schedule_seen = False
        self.schedule_equal = False
        self.schedule_different = False

        self.codegen_seen = False

        scops.append(self)

        found = False
        for suite, l in suites.items():
            if suite in folder:
                l.append(self)
                found = True
                break
        if not found:
          suites['llvm-ts'].append(self)

    def __str__(self):
        return 'AL/BL: %s/%s | VP/AP: %s/%s | PRU [s %i,p %i,u %i] | AST [s %i,e %i,d %i] | SCH [s %i,e %i,d %i]' % (str(self.als), str(self.bls), str(self.vps), str(self.aps), self.prune_seen, self.prune_profitable, self.prune_unprofitable, self.ast_seen, self.ast_equal, self.ast_different, self.schedule_seen, self.schedule_equal, self.schedule_different)

    def addLine(self, line):
        # print(self.name, getName(line))
        if 'AST:' in line:
            assert 'Transformed' in line or 'Original' in line
            self.asts.append([])
            return True

        assert getName(line) == self.name
        if 'START' in line or 'CODEGEN' in line:
            al, bl, vp, ap =getAlBlVpAp(line)
            self.als.append(al)
            self.bls.append(bl)
            self.vps.append(vp)
            self.aps.append(ap)
        if 'PRUNE START' in line:
            assert not self.prune_seen
            self.prune_seen = True
            return False
        if 'PRUNE' in line:
            assert not self.prune_profitable and not self.prune_unprofitable
            if 'UNPROFITABLE' in line:
                self.prune_unprofitable = True
            else:
                self.prune_profitable = True
            return False
        if 'AST START' in line:
            assert not self.ast_seen
            self.ast_seen = True
            return False
        if 'AST' in line:
            assert not self.ast_equal and not self.ast_different
            if 'EQUAL' in line:
                self.ast_equal = True
            elif 'DIFFERENT' in line:
                self.ast_different = True
            return False
        if 'SCHEDULE START' in line:
            assert not self.schedule_seen
            self.schedule_seen = True
            return False
        if 'SCHEDULE' in line:
            assert not self.schedule_equal and not self.schedule_different
            if 'EQUAL' in line:
                self.schedule_equal = True
            elif 'DIFFERENT' in line:
                self.schedule_different = True
            return False
        if 'CODEGEN' in line:
            assert not self.codegen_seen
            self.codegen_seen = True
            return False

        print(line)
        assert False and "UNKNOWN LINE!"


folder = None
read_non_empty = False
last_scop = None
with open(sys.argv[1], 'r') as fd:
    for line in fd:
        if line.startswith('cd /') and 'sandbox' in line and '&&' in line:
            folder = line[3:line[3:].index(' ') + 3][len('/home/buildslave/Polly_corr/sandbox/test-2018-05-03_21-59-23/'):]
            continue
        if not line.startswith('PME: '):
            continue
        line = line[4:]
        if 'SCOP START' in line:
            # print("found scop: ", getName(line), getAlBlVpAp(line))
            last_scop = Scop(getName(line), folder, *getAlBlVpAp(line))
            continue

        # print(read_non_empty, line)
        assert last_scop
        if read_non_empty:
            if not line.strip():
                read_non_empty = False
            else:
                assert len(last_scop.asts) > 0
                last_scop.asts[-1].append(line)
        else:
            read_non_empty = last_scop.addLine(line)

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

for suite, scops in unified_suites.items():
    schedule_equal_and_predicted = 0
    schedule_equal_and_mispredicted = 0
    schedule_different_and_predicted = 0
    schedule_different_and_mispredicted = 0
    ast_equal_and_predicted = 0
    ast_equal_and_mispredicted = 0
    ast_different_and_predicted = 0
    ast_different_and_mispredicted = 0
    scops_dismissed_ast = 0
    scops_pruned = 0
    scops_not_pruned = 0
    scops_never_pruned = 0
    scops_dismissed_sched = 0
    for scop in scops:
        if scop.prune_unprofitable:
            scops_pruned += 1
        elif scop.prune_profitable:
            scops_not_pruned += 1
        else:
            scops_never_pruned += 1
        if not scop.schedule_equal and not scop.schedule_different:
            scops_dismissed_sched += 1
            continue
        if scop.schedule_equal:
            if sum(scop.vps) == 0:
                assert 0
            elif sum(scop.aps) == 0:
                schedule_equal_and_predicted += 1
            else:
                schedule_equal_and_mispredicted += 1
        else:
            assert scop.schedule_different
            if sum(scop.vps) == 0:
                assert 0
            elif sum(scop.aps) > 0:
                schedule_different_and_predicted += 1
            else:
                schedule_different_and_mispredicted += 1

        if not scop.ast_equal and not scop.ast_different:
            scops_dismissed_ast += 1
            continue
        if scop.ast_equal:
            if sum(scop.vps) == 0:
                assert 0
            elif sum(scop.aps) == 0:
                ast_equal_and_predicted += 1
            else:
                ast_equal_and_mispredicted += 1
        else:
            assert scop.ast_different
            if sum(scop.vps) == 0:
                assert 0
            elif sum(scop.aps) > 0:
                ast_different_and_predicted += 1
            else:
                ast_different_and_mispredicted += 1
                print(scop)
                assert len(scop.asts) == 2
                l0 = len(scop.asts[0])
                l1 = len(scop.asts[1])
                for i in range(max(l0, l1)):
                    line0, line1 = ' ', ' '
                    if i < l0:
                        line0 = scop.asts[0][i][:-1]
                    if i < l1:
                        line1 = scop.asts[1][i][:-1]
                    print('%s | %s' % (line0.ljust(60), line1.ljust(60)))


    print("\n\nSUITE: ",suite, '(%i)' % len(scops))
    print("Profitable:", scops_not_pruned)
    print("    PRUNED:", scops_pruned)
    print("Not PRUNED:", scops_never_pruned)
    print("Dismissed SCHEDULE:", scops_dismissed_sched)
    print("Dismissed      AST:", scops_dismissed_ast)
    print("SCHEDULE Equal and aps predicted:", schedule_equal_and_predicted)
    print("SCHEDULE Equal and aps mispredicted:", schedule_equal_and_mispredicted)
    print("SCHEDULE Different and aps predicted:", schedule_different_and_predicted)
    print("SCHEDULE Different and aps mispredicted:", schedule_different_and_mispredicted)
    print("AST Equal and aps predicted:", ast_equal_and_predicted)
    print("AST Equal and aps mispredicted:", ast_equal_and_mispredicted)
    print("AST Different and aps predicted:", ast_different_and_predicted)
    print("AST Different and aps mispredicted:", ast_different_and_mispredicted)

    n = len(scops) - scops_dismissed_sched
    print("schedule", n)

    m = schedule_different_and_predicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c2,' % (p, p, m))
    m = schedule_different_and_mispredicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c3,' % (p, p, m))

    m = schedule_equal_and_predicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c0,' % (p, p, m))
    m = schedule_equal_and_mispredicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c1%%' % (p, p, m))

    n = n - scops_dismissed_ast
    print("ast", n)

    m = ast_different_and_predicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c2,' % (p, p, m))
    m = ast_different_and_mispredicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c3,' % (p, p, m))

    m = ast_equal_and_predicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c0,' % (p, p, m))
    m = ast_equal_and_mispredicted
    p = m * 100.0 / n
    print('%.4f/%.1f\\%% (%i)/c1%%' % (p, p, m))
