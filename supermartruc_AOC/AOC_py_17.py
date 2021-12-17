def transfo(string):
    try:
        return int(string)
    except ValueError:
        return int(string[:-1])


class ShootError(Exception):
    pass


[xmin, xmax], [ymin, ymax] = [list(map(transfo, coor[2:].split('..'))) for coor in
                              open("AOC_txt_17").read().split(" ")[2:]]


def height(vx, vy):
    x, y = 0, 0
    pos_atteinte = [(0, 0)]
    while x <= xmax and y >= ymin:
        x += vx
        y += vy
        vy -= 1
        if vx > 0:
            vx -= 1
        elif vx < 0:
            vx += 1
        pos_atteinte.append((x, y))
        if xmax >= x >= xmin and ymin <= y <= ymax:
            return max([pos[1] for pos in pos_atteinte])
    raise ShootError


# PART 1
vxt = vyt = 0
maxh = ymin
while vyt <= -ymin:
    vxt = 0
    while vxt <= xmax:
        try:
            maxh = max(maxh, height(vxt, vyt))
            vxt += 1
        except ShootError:
            vxt += 1
    vyt += 1
print(maxh)

# PART 2
vxt = 0
vyt = ymin
ncouple = 0
while vyt <= -ymin:
    vxt = 0
    while vxt <= xmax:
        ncouple += 1
        try:
            height(vxt, vyt)
            vxt += 1
        except ShootError:
            ncouple -= 1
            vxt += 1
    vyt += 1
print(ncouple)
