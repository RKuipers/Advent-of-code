ps = list(map(str.splitlines, open('inputs/day_13_large.txt').read().split('\n\n')))

def f(p, s):
    for i in range(1, len(p)):
        if sum(c1 != c2 for r1, r2 in zip(p[i-1::-1], p[i:])
                        for c1, c2 in zip(r1, r2)) == s: return i
    else: return 0

for s in 0, 1: print(list(100 * f(p,s) + f([*zip(*p)],s) for p in ps))
