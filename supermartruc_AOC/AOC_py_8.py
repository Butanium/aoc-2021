# PART 1
# print(sum([sum([(len(s) in [2, 3, 4, 7]) for s in li.split(" | ")[1].split()]) for li in
#            open("AOC_txt_8").read().split("\n")]))

# Part 2
data = [[s.split() for s in li.split(' | ')] for li in open("AOC_txt_8").read().split("\n")]
spat = [sorted(s[0], key=lambda li: len(li)) for s in data]
outs = [s[1] for s in data]
seg = [['' for i in range(7)] for k in range(len(data))]

def decode(wires, number):
    num = ''.join(sorted([str(wires.index(letter)) for letter in number]))
    if num == '012456':
        return '0'
    if num == '25':
        return '1'
    if num == '02346':
        return '2'
    if num == '02356':
        return '3'
    if num == '1235':
        return '4'
    if num == '01356':
        return '5'
    if num == '013456':
        return '6'
    if num == '025':
        return '7'
    if num == '0123456':
        return '8'
    if num == '012356':
        return '9'
    else: return 'erreur'

for k in range(len(spat)):
    pat = spat[k]
    seg[k][0] = [c for c in pat[1] if c not in pat[0]][0]
    seg[k][6] = [c for c in pat[9] if (c not in (''.join(pat[:3])) and (''.join(pat[3:]).count(c) == len(pat[3:])))][0]
    seg[k][2] = [c for c in pat[0] if (sum([1 for j in range(6, 9) if c in pat[j]]) == 2)][0]
    seg[k][5] = [c for c in pat[0] if c != seg[k][2]][0]
    seg[k][3] = [c for c in pat[9] if (''.join(pat[3:6]).count(c) == 3 and c != seg[k][0] and c != seg[k][6])][0]
    seg[k][1] = [c for c in pat[2] if c not in seg[k]][0]
    seg[k][4] = [c for c in 'abcdefg' if c not in seg[k]][0]

res = [int(''.join([decode(seg[j],outs[j][k]) for k in range(4)])) for j in range(len(outs))]

print(sum(res))