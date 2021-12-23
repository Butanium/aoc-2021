import time
t = time.time()
pos1, pos2 = [int(line[-1]) for line in open("AOC_txt_21").read().split("\n")]

# PART 1
"""
s1 = 0
s2 = 0
dice = 1
dcount = 0
while not (s1 >= 1000 or s2 >= 1000):
    pos1 = (3 * (dice + 1) + pos1 - 1) % 10 + 1
    dice = (dice + 2) % 100 + 1
    dcount += 3
    s1 += pos1
    if s1 >= 1000:
        print(dcount * s2)
    else:
        pos2 = (3 * (dice + 1) + pos2 - 1) % 10 + 1
        dice = (dice + 2) % 100 + 1
        dcount += 3
        s2 += pos2
        if s2 >= 1000:
            print(dcount * s1)
"""
# [whoturn,s1,s2,p1,p2] : (w1,w2)
# PART 2
cases = {}


def sumt(x):
    a, b, c, d, e, f, g = x
    return (a[0] + 3 * b[0] + 6 * c[0] + 7 * d[0] + 6 * e[0] + 3 * f[0] + g[0],
            a[1] + 3 * b[1] + 6 * c[1] + 7 * d[1] + 6 * e[1] + 3 * f[1] + g[1])


def solve(key):
    if key[1] >= 21:
        cases[key] = (1, 0)
    elif key[2] >= 21:
        cases[key] = (0, 1)
    if key in cases:
        return cases[key]
    [wt, s1, s2, p1, p2] = key
    if wt == 0:
        c3 = (1, s1 + (p1 + 2) % 10 + 1, s2, (p1 + 2) % 10 + 1, p2)
        c4 = (1, s1 + (p1 + 3) % 10 + 1, s2, (p1 + 3) % 10 + 1, p2)
        c5 = (1, s1 + (p1 + 4) % 10 + 1, s2, (p1 + 4) % 10 + 1, p2)
        c6 = (1, s1 + (p1 + 5) % 10 + 1, s2, (p1 + 5) % 10 + 1, p2)
        c7 = (1, s1 + (p1 + 6) % 10 + 1, s2, (p1 + 6) % 10 + 1, p2)
        c8 = (1, s1 + (p1 + 7) % 10 + 1, s2, (p1 + 7) % 10 + 1, p2)
        c9 = (1, s1 + (p1 + 8) % 10 + 1, s2, (p1 + 8) % 10 + 1, p2)
        cases[key] = sumt((solve(c3), solve(c4), solve(c5), solve(c6), solve(c7), solve(c8), solve(c9)))
    else:
        c3 = (0, s1, s2 + (p2 + 2) % 10 + 1, p1, (p2 + 2) % 10 + 1)
        c4 = (0, s1, s2 + (p2 + 3) % 10 + 1, p1, (p2 + 3) % 10 + 1)
        c5 = (0, s1, s2 + (p2 + 4) % 10 + 1, p1, (p2 + 4) % 10 + 1)
        c6 = (0, s1, s2 + (p2 + 5) % 10 + 1, p1, (p2 + 5) % 10 + 1)
        c7 = (0, s1, s2 + (p2 + 6) % 10 + 1, p1, (p2 + 6) % 10 + 1)
        c8 = (0, s1, s2 + (p2 + 7) % 10 + 1, p1, (p2 + 7) % 10 + 1)
        c9 = (0, s1, s2 + (p2 + 8) % 10 + 1, p1, (p2 + 8) % 10 + 1)
        cases[key] = sumt((solve(c3), solve(c4), solve(c5), solve(c6), solve(c7), solve(c8), solve(c9)))
    return cases[key]


solve((0, 0, 0, pos1, pos2))

print(cases[(0, 0, 0, pos1, pos2)])
print(time.time() - t)

