import sys

if len(sys.argv) < 2:
    sys.exit(1)

suites = {'SPEC/CINT2000':[], 'SPEC/CFP2000':[], 'SPEC/CINT2006': [], 'SPEC/CFP2006': [], 'SPEC/CINT2017': [], 'SPEC/CFP2017':[], 'llvm-ts': []}
stats = {}
scops = {}
all_scops = []

def getId(line):
    assert ' | ' in line
    name = line[line.index(' | ') + 3:]
    return name.strip()

class SCoP:
    def __init__(self, id, folder):
        self.id = id
        self.infos = {}
        self.folder = folder
        scops[id] = self
        all_scops.append(self)

        found = False
        for suite, l in suites.items():
            if suite in folder:
                l.append(self)
                found = True
                break
        if not found:
          suites['llvm-ts'].append(self)



    def __str__(self):
        return 'SCoP: %s [%s][#I: %i]' % (self.id, self.folder, len(self.infos))

    def addLine(self, line, key_prefix = ''):
        assert ':' in line
        fatal = None
        if line.count(':') > 1:
            assert '[' in line and ']' in line
            fatal = line[line.index('[') + 1 : line.index(']')].lower()
            line = line[:line.index('[')]

        assert line.count(':') == 1
        idx = line.index(':')
        key = key_prefix + line[:idx]
        val = int(line[idx + 1:].strip())
        self.infos[key] = val

        if fatal:
            self.addLine(fatal, key + ' ')



folder = None
last_scop = None
read_non_empty = False
with open(sys.argv[1], 'r') as fd:
    for line in fd:
        if line.startswith('cd /') and 'sandbox' in line and '&&' in line:
            folder = line[3:line[3:].index(' ') + 3][len('/home/buildslave/Polly_corr/sandbox/test-2018-05-03_21-59-23/'):]
            continue
        if not line.startswith('CEV '):
            continue

        line = line[4:].strip()
        if 'New SCoP ' in line:
            last_scop = SCoP(getId(line), folder)
            continue

        if ':' in line:
            assert last_scop
            last_scop.addLine(line)
            continue

        id = getId(line)
        scop = scops[id]
        if 'SCoP feasible' in line:
            scop.infos['feasible'] = True
            continue
        elif 'SCoP infeasible' in line:
            scop.infos['feasible'] = False
            continue
        elif 'pruned' in line:
            scop.infos['pruned'] = 'not pruned' not in line
            continue
        elif 'dependences' in line:
            scop.infos['dependences'] = 'no dependences' not in line
            continue
        elif 'new schedule' in line:
            scop.infos['scheduled'] = 'no profitable' not in line
            continue
        elif 'ast' in line:
            if 'doesn' in line:
                continue
            if 'not build' in line:
                scop.infos['ast'] = False
                continue
            if 'will be' in line:
                scop.infos['ast'] = True
                continue
        elif 'inv preload' in line:
            scop.infos['inv preload'] = 'done' in line
            scop.infos['inv preload fail'] = 'fail' in line
            continue


        print('UNKOWN line: ', line)
        assert False


# for scop in all_scops:
    # print(scop.infos)

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

    # folders = set()
    # for scop in l:
        # folders.add(scop.folder)
    # print(suite)
    # lf = list(folders)
    # lf.sort()
    # for folder in lf:
        # print('  ', folder)


fatal_keys = {}
for suite, l in unified_suites.items():
    num_scops = len(l)
    feasible_scops = 0
    twol_oned = 0
    twol_twod = 0
    loops_map = {}
    for i in range(100):
        loops_map[i] = 0
    depth_map = {}
    for i in range(10):
        depth_map[i] = 0
    depth_map_no_invload = {}
    depth_map_no_nonaffinsubreg = {}
    depth_map_no_aliasing = {}
    depth_map_no_unsigned = {}
    depth_map_no_wrapping = {}
    depth_map_no_error = {}
    for i in range(10):
        for d in [depth_map_no_invload, depth_map_no_nonaffinsubreg,
                  depth_map_no_aliasing, depth_map_no_unsigned, depth_map_no_wrapping,depth_map_no_error]:
            d[i] = 0
    stmts_map = {}
    for i in range(100):
        stmts_map[i] = 0

    fatals = {}
    for scop in l:
        if not 'feasible' in scop.infos:
            continue
        if not scop.infos['feasible']:
            fatal_found = False
            for k, v in scop.infos.items():
                if not k.startswith('assumption') or not k.endswith('fatal'):
                    continue
                if k not in fatals:
                    fatals[k] = 0
                if not v:
                    continue
                fatal_found = True
                fatals[k] += 1
            # if not fatal_found:
                # print(scop)
                # for k, v in scop.infos.items():
                    # print(k, v)
                # assert False
            continue
        mld = scop.infos['Max loop depth']
        if mld == 0:
            continue
        # if mld > 2:
            # print(scop)
        feasible_scops += 1
        nl = scop.infos['#loops']
        loops_map[nl] += 1
        depth_map[mld] += 1
        if not scop.infos['#req inv loads']:
            depth_map_no_invload[mld] += 1
        if not scop.infos['#non-affine subregions']:
            depth_map_no_nonaffinsubreg[mld] += 1
        if not scop.infos['#alias groups']:
            depth_map_no_aliasing[mld] += 1
        if not scop.infos['#unsigned comparisons']:
            depth_map_no_unsigned[mld] += 1
        if scop.infos['#wrapping operations'] <= scop.infos['#wrapping operations safe']:
            depth_map_no_wrapping[mld] += 1
        if not scop.infos['#error blocks']:
            depth_map_no_error[mld] += 1
        stmts_map[scop.infos['#stmts']] += 1

        if nl == 2:
            if mld == 2:
                twol_twod += 1
            else:
                assert mld == 1
                twol_oned += 1

    print(suite)
    print('num scops:', num_scops)
    print('feasible scops:', feasible_scops)
    print('infeasible scops:', num_scops - feasible_scops)
    twol = twol_twod + twol_oned
    pt = twol_twod * 100.0 / twol
    po = twol_oned * 100.0 / twol
    print('two loops one depth:', twol_oned, '%.4f/%.1f\\%%\\\\[-2mm](%i)/c0' % (po, po, twol_oned))
    print('two loops two depth:', twol_twod, '%.4f/%.1f\\%% (%i)/c2' % (pt, pt, twol_twod))

    print('depths:')
    for d,n in depth_map.items():
        if n:
            print('depth %i: %i (%.5f)' % (d, n, n * 100.0 / feasible_scops))
    print('depths (no wrapping):')
    for d,n in depth_map_no_wrapping.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
    print('depths (no inv load):')
    for d,n in depth_map_no_invload.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
    print('depths (no non-aff subregion):')
    for d,n in depth_map_no_nonaffinsubreg.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
    print('depths (no aliasing ptrs):')
    for d,n in depth_map_no_aliasing.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
    print('depths (no unsigned):')
    for d,n in depth_map_no_unsigned.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
    no_error_sum = 0
    print('depths (no error blocks):')
    for d,n in depth_map_no_error.items():
        if n:
            print('depth %i: %i (%.1f)' % (d, n, n * 100.0 / depth_map[d]))
            no_error_sum += n
    print('no error #scops: %.1f' % (no_error_sum * 100.0 / feasible_scops))
    # print("stmts:")
    # for d,n in stmts_map.items():
        # if n:
            # print('(%i, %i)' % (d, n))
    # print("loops:")
    # for d,n in loops_map.items():
        # if n:
            # # print('loops %i: %i (%.5f)' % (d, n, n * 100.0 / feasible_scops))
            # print('(%i, %i)' % (d, n))

    n = 1
    v_all = 0
    p_all = 0
    for k, v in fatals.items():
        v_all += v
        p = v * 100.0 / num_scops
        p_all += p
        if v:
            print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, v, n, k))
            if k in fatal_keys:
                assert fatal_keys[k] == n
            fatal_keys[k] = n
        n += 1

    p = feasible_scops * 100.0 / num_scops
    p_all += p
    v_all += feasible_scops
    print('%.5f/%.1f\\%% (%i)/c%i, %% %s' % (p, p, feasible_scops, 0, 'feasible'))
    fatal_keys['feasible'] = 0

    p = 100 - p_all
    v = num_scops - v_all
    print('%.5f/%.1f\\%% (%i)/c%i%%  %s' % (p, p, v, n, 'multiple'))
    fatal_keys['multiple'] = n

    print('\np_all:', p_all, ' (', (100 - p_all), ')')
    print('v_all:', v_all, ' (', (num_scops - v_all), ')')


# for k, n in fatal_keys.items():
    # print('\\node[c%i, legend box]{}; & %s \\\\' % (n, k))


print('\n\n')

loop_scores_base = {}

for key in ['','#error blocks', '#unsigned comparisons', '#alias groups', '#req inv loads',
            '#non-affine subregions', ('#wrapping operations', '#wrapping operations safe')]:
    for suite, l in unified_suites.items():
        fatals = {}
        loop_scores = [0, 0, 0, 0]
        stmt_scores = [0, 0, 0, 0]
        loop_scops = [0, 0, 0, 0]
        stmt_scops = [0, 0, 0, 0]
        for scop in l:
            if not 'feasible' in scop.infos:
                continue
            if not scop.infos['feasible']:
                continue
            if key:
                if type(key) == str:
                    if scop.infos[key]:
                        continue
                else:
                    assert type(key) == tuple and len(key) == 2
                    if scop.infos[key[0]] > scop.infos[key[1]]:
                        continue
            for i in range(4):
                mld = scop.infos['Max loop depth']
                ns = scop.infos['#stmts']
                if mld > i:
                    loop_scores[i] += max(0, mld - i)
                    loop_scops[i] += 1
                if ns > i:
                    stmt_scores[i] += max(0, ns - i)
                    stmt_scops[i] += 1

        print('suite [%s]: %s' % (key, suite))
        for i in range(2):
            if key:
                print('i', i, ' loop score', loop_scores[i], ' #scops', loop_scops[i], ' q:', loop_scores[i] / loop_scops[i] if loop_scops[i] else 'n/a', ' ls/base= %.1f' % (loop_scores[i] * 100 / loop_scores_base[suite][i]))
            else:
                print('i', i, ' loop score', loop_scores[i], ' #scops', loop_scops[i], ' q:', loop_scores[i] / loop_scops[i] if loop_scops[i] else 'n/a')
            # print('i', i, ' stmt score', stmt_scores[i], ' #scops', stmt_scops[i], ' q:', stmt_scores[i] / stmt_scops[i] if stmt_scops[i] else 'n/a')
            if not key:
                loop_scores_base[suite] = loop_scores


for suite, l in unified_suites.items():
    m = {}
    m['coalesced'] = (0, 0)
    nf = 0
    for scop in l:
        if 'feasible' in scop.infos and scop.infos['feasible']:
            nf += 1
            ilcv = scop.infos['#inv loads classes']
            ilv = scop.infos['#inv loads']
            if ilcv < ilv:
                k = 'coalesced'
                m[k] = (m[k][0] + ilv - ilcv, m[k][1] + 1)
        for k, v in scop.infos.items():
            if not 'fatal' in k:
                if not 'feasible' in scop.infos:
                    continue
                if not scop.infos['feasible']:
                    continue
            if not 'inv' in k:
                continue
            if not v:
                continue
            if k == 'inv preload':
                if not scop.infos['#inv loads']:
                    k = 'no inv preload'
                    if not k in m:
                        m[k] = (0, 0)
                    m[k] = (m[k][0] + v, m[k][1] + 1)
                    continue
            if not k in m:
                m[k] = (0, 0)
            m[k] = (m[k][0] + v, m[k][1] + 1)
    print(suite, len(l), nf)
    for k, v in m.items():
        print(k, v[0], v[1], '%.1f%%' % (v[1] * 100.0 / nf))


for suite, l in unified_suites.items():
    m = {}
    nf = 0
    noassscops = 0
    for scop in l:
        if not scop.infos['feasible']:
            continue
        NoAss = True
        nf += 1
        for k, v in scop.infos.items():
            if (not k.startswith('assumption') or k.endswith('fatal')) and not k == '#alias groups':
                continue
            if not v:
                continue
            NoAss = False
            if k in m:
                m[k] += 1
            else:
                m[k] = 1
        if NoAss:
            noassscops += 1

    print(suite, len(l), nf, noassscops)
    for k, v in m.items():
        print(k, v, '%.1f%%' % (v * 100.0 / nf))

