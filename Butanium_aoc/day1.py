file = open("puzzle_input/day1.txt", 'r')
lines = list(map(int, file.readlines()))


def solve(measures):
    acc = measures[0]
    r = 0
    for i in measures:
        if i > acc:
            r += 1
        acc = i
    return r


print("result for part 1 : %d" % solve(lines))

measurements = list(sum(lines[i - 1:i + 2]) for i in range(1, len(lines) - 1))
print("result for part 2 : %d" % solve(measurements))
