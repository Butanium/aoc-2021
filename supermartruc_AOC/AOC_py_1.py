l = list(map(int,open("AOC_txt_1").read().split("\n")))

"""PART 1 
c = 0
for k in range(len(l)-1):
    if l[k+1]>l[k]: c+=1
print(c)"""

"""PART 2
c=0
for k in range(len(l)-3):
    if l[k+3] > l[k]: c+=1
print(c)
"""