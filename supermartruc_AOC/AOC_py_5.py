l = [[line1[0].split(','), line1[1].split(',')] for line1 in
     [line.split(" -> ") for line in open("AOC_txt_5").read().split("\n")]]
cp1x = [line for line in l if line[0][0] == line[1][0]]
cp1y = [line for line in l if line[0][1] == line[1][1]]
cp2d = [line for line in l if (line not in cp1x) and (line not in cp1y)]
points = [[0 for i in range(991)] for j in range(991)]
for couple in cp1x:
    [x1, y1], [x2, y2] = couple
    [x1, y1], [x2, y2] = [int(x1), int(y1)], [int(x2), int(y2)]
    for k in range(min(y1, y2), max(y1, y2) + 1):
        points[k][x1] += 1
for couple in cp1y:
    [x1, y1], [x2, y2] = couple
    [x1, y1], [x2, y2] = [int(x1), int(y1)], [int(x2), int(y2)]
    for k in range(min(x1, x2), max(x1, x2) + 1):
        points[y1][k] += 1
for couple in cp2d:
    [x1, y1], [x2, y2] = couple
    [x1, y1], [x2, y2] = [int(x1), int(y1)], [int(x2), int(y2)]
    if x1<x2:
        [xm,ym] = [x1,y1]
        [xM,yM] = [x2,y2]
    else:
        [xm, ym] = [x2,y2]
        [xM,yM] = [x1,y1]
    if ym<yM: epsilon = 1
    else: epsilon = -1
    for k in range(xM-xm+1):
        points[ym + epsilon*k][xm+k] += 1


c = 0
for i in range(len(points)):
    for j in range(len(points[0])):
        if points[i][j] >= 2:
            c += 1
print(c)
