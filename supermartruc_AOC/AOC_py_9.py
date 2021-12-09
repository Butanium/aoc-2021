hmap = ['9' + line + '9' for line in open("AOC_txt_9").read().split("\n")]
hmap = ['9' * len(hmap[0])] + hmap + ['9' * len(hmap[0])]
low_points = [hmap[i][j] for j in range(1, len(hmap[0]) - 1) for i in range(1, len(hmap) - 1)
              if hmap[i][j] < min(hmap[i - 1][j], hmap[i + 1][j], hmap[i][j + 1], hmap[i][j - 1])]

# print(sum([1 + int(lp) for lp in low_points]))  # PART 1

lp_index = [(i, j) for j in range(1, len(hmap[0]) - 1) for i in range(1, len(hmap) - 1)
            if hmap[i][j] < min(hmap[i - 1][j], hmap[i + 1][j], hmap[i][j + 1], hmap[i][j - 1])]


def find_neigh(point):
    i, j = point
    return [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)]


def find_bassin_neigh(stack, point, b):
    stack.append(point)
    hmap[point[0]][point[1]] += '*'
    p_value = int(hmap[point[0]][point[1]][0])
    for p in find_neigh(point):
        n_value = int(hmap[p[0]][p[1]][0])
        if n_value != 9 and p_value < n_value and hmap[p[0]][p[1]][-1] != '*':
            find_bassin_neigh(stack, p, False)
    if b:
        return stack

hmap = [[k for k in line] for line in hmap]

# print(lp_index)

basin = [find_bassin_neigh([], point, True) for point in lp_index]

# for line in hmap:
#     print(line)

# print(basin)
l_basin = sorted([len(k) for k in basin])[::-1]
print(l_basin)
print(l_basin[0] * l_basin[1] * l_basin[2])
