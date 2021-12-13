points = [[int(k) for k in c.split(',')] for c in open("AOC_txt_13").read().split("\n")[:897]]  # 897
folds = [line.split(' ')[-1] for line in open("AOC_txt_13").read().split("\n")[898:]]           #898
print(points)

matrice = [['.' for i in range(1311)] for j in range(895)]  # 895 1311

for i, j in points:
    matrice[j][i] = '#'

print(folds)


def fold(m, axis):
    n = int(axis[2:])
    if axis[0] == 'y':
        res = [m[k].copy() for k in range(n)]
        for i in range(n):
            for j in range(len(m[0])):
                res[i][j] = (res[i][j] == '#' or m[2 * n - i][j] == '#') * '#' + (
                    not (res[i][j] == '#' or m[2 * n - i][j] == '#')) * '.'
    else:
        res = [m[k].copy()[:n] for k in range(len(m))]
        for i in range(len(m)):
            for j in range(n):
                res[i][j] = (res[i][j] == '#' or m[i][2*n - j] == '#') * '#' + (
                    not (res[i][j] == '#' or m[i][2*n - j] == '#')) * '.'

    return res


# m2 = fold(matrice, folds[0])
# print(sum([line.count("#") for line in m2]))

for f in folds:
    matrice = fold(matrice, f)


for line in matrice:
    print(''.join(line))
