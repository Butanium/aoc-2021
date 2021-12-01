
file = open("puzzle_input/day1.txt",'r')
lines = file.readlines()
acc = int(lines[0])
r = 0
for i in lines:
    if int(i) > acc:
        r += 1
    acc = int(i)

print(r)