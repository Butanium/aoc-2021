"""PART 1
liste = [[t[k] for t in open("AOC_txt_3").read().split("\n")] for k in range(12)]
gamma = ''.join([str(int(k.count('0') < k.count('1'))) for k in liste])
epsilon = ''.join([str(1-int(k)) for k in gamma])
print(int(gamma,2)*int(epsilon,2))"""

"""PART 2
liste = open("AOC_txt_3").read().split("\n"); k=0; oxys=liste[:]; co2=liste[:]
while len(oxys) > 1 or len(co2) > 1:
    oxys = [nb for nb in oxys if (int(nb[k]) == ([o[k] for o in oxys].count('1') >= len(oxys)/2)) or len(oxys)==1]
    co2 = [nb for nb in co2 if (int(nb[k]) == ([c[k] for c in co2].count('0') > len(co2)/2)) or len(co2)==1]
    k+=1
print(int(oxys[0],2)*int(co2[0],2))
"""