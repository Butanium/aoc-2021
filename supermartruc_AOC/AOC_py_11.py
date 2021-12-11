octop = [[int(octo) for octo in line] for line in open("AOC_txt_11").read().split("\n")]
flashes = 0


def add(f):
    for i in range(len(f)):
        for j in range(len(f[0])):
            f[i][j] += 1


def voisins(f, i, j):
    n = len(f)
    liste = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j + 1), (i + 1, j + 1), (i + 1, j), (i + 1, j - 1),
             (i, j - 1)]
    return [vois for vois in liste if 0 <= vois[0] <= n - 1 and 0 <= vois[1] <= n - 1]


def update(f):
    add(f)
    n = len(f)
    flash = 0
    flashing = [(i, j) for i in range(n) for j in range(n) if f[i][j] > 9]
    while flashing:
        flash += 1
        i, j = flashing.pop()
        f[i][j] = 0
        v = [(o, k) for o, k in voisins(f, i, j) if f[o][k] != 0]
        for o, k in v:
            f[o][k] += 1
            if f[o][k] > 9 and (o, k) not in flashing:
                flashing.append((o, k))
    return flash


# PART 1
# for _ in range(100):
#     flashes += update(octop)

# print(flashes)


# PART 2
# k=0
# while octop != [[0]*len(octop)]*len(octop):
#     k+=1
#     update(octop)
# print(k)
