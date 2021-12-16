import time

timing = time.time()
liste = open("AOC_txt_15").read().split("\n")
n = len(liste)

dist = {}
mat = {}

for i in range(n):
    for j in range(n):
        mat[(i, j)] = int(liste[i][j])
        dist[(i, j)] = 1000000
dist[(n - 1, n - 1)] = 0
dist[(n - 2, n - 1)] = mat[(n - 1, n - 1)]
dist[(n - 1, n - 2)] = mat[(n - 1, n - 1)]


def voisins(c):
    m, o = c
    return [k for k in [(m - 1, o), (m + 1, o), (m, o - 1), (m, o + 1)] if 0 <= k[0] <= n - 1 and 0 <= k[1] <= n - 1]


t = [(n - 1, n - 1)]
r = [c for c in dist.keys() if c != (n - 1, n - 1)]
frontiere = [(n - 2, n - 1), (n - 1, n - 2)]
while not (0, 0) in t:
    x = frontiere[0]
    md = dist[x]
    for elt in frontiere:
        if dist[elt] < md:
            x = elt
            md = dist[x]
    t.append(x)
    r.remove(x)
    frontiere.remove(x)
    for y in voisins(x):
        if dist[y] == 1000000:
            frontiere.append(y)
        if mat[x] + md < dist[y]:
            dist[y] = mat[x] + md

print(time.time() - timing)
