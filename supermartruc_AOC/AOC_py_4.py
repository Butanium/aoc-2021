l = open("AOC_txt_4").read().split("\n")
l2 = open("AOC_txt_4").read().split("\n\n")[1:]

drawn = l[0].split(',')
boards = [[row.split() for row in b.split("\n")] for b in l2]
print(boards)
check = lambda board: (sum([[nb[-1] for nb in row] == 5 * ['*'] for row in board]) + sum(
    [[row[k][-1] for row in board] == 5 * ['*'] for k in range(5)])) > 0
sunmark = lambda board: sum([sum([int(nb) for nb in row if nb[-1] != '*']) for row in board])
mark = lambda nbd, boards: [[[nb + (nbd == nb) * '*' for nb in row] for row in board] for board in
                            boards]

n = 0
""" PART 1 """
while [check(b) for b in boards]==len(boards)*[False]: boards=mark(drawn[n],boards); n+=1
print(int(drawn[n-1])*sunmark(boards[[check(b) for b in boards].index(True)]))

""" PART 2 """
while sum([check(b) for b in boards]) != len(boards) - 1: boards = mark(drawn[n], boards); n += 1  # PART 2
x = [boards[[check(b) for b in boards].index(False)]]
while not check(x[0]): x = mark(drawn[n], x); n += 1;
print(int(drawn[n - 1]) * sunmark(x[0]))
