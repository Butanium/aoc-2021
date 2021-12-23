table = open("AOC_txt_20").read().split("\n")[0]
image = [['.'] * 103 + [elt for elt in line] + ['.'] * 103 for line in open("AOC_txt_20").read().split("\n")[2:]]
long = len(image)
image = 103 * [(long + 206) * ['.']] + image + 103 * [(long + 206) * ['.']]



def to_lit(im, pixel, n):
    i, j = pixel
    nei = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j), (i, j + 1), (i + 1, j - 1), (i + 1, j),
           (i + 1, j + 1)]
    res = ''
    for vois in nei:
        try:
            res += str(int(im[vois[0]][vois[1]] == '#'))
        except IndexError:
            if n%2 == 0:
                res += '0'
            else:
                res += '1'

    return table[int(res, 2)]


def counting(im):
    return sum([sum([pixel == '#' for pixel in line]) for line in im])


new_image = [[item for item in line] for line in image]


for k in range(50):
    for i in range(len(image)):
        for j in range(len(image[0])):
            new_image[i][j] = to_lit(image, (i, j), k)
    image = [[item for item in line] for line in new_image]

print(counting(image))
