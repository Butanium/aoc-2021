l = open("AOC_txt_2").read().split("\n")

""" PART 1
f = sum([int(k[-1]) for k in l if k[0]=='f'])
u = sum([int(k[-1]) for k in l if k[0]=='u'])
d = sum([int(k[-1]) for k in l if k[0]=='d'])

print(f*(d-u))
"""

"""PART 2

aim=0
hpos = 0
depth = 0
for k in l:
    if k[0]=='f': hpos += int(k[-1]); depth += aim*int(k[-1])
    elif k[0]=='u': aim -= int(k[-1])
    else: aim += int(k[-1])
print(hpos*depth)
"""