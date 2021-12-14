chaine = open("AOC_txt_14").read().split("\n")[0]
occu = {letter: chaine.count(letter) for letter in chaine}
rules = {r[0]: r[1] for r in [rule.split(" -> ") for rule in open("AOC_txt_14").read().split("\n")[2:]]}

pairs = {}
for i in occu.keys():
    for j in occu.keys():
        pairs[i + j] = chaine.count(i+j)

# def next_tab(tab):
#     res = ['']*(2*len(tab)-1)
#     for k in range(2*len(tab)-1):
#         if not k%2:
#             res[k] = tab[k//2]
#         else:
#             try:
#                 res[k] = rules[tab[k//2] + tab[k//2 + 1]]
#             except ValueError:
#                 res[k] = ''
#     return res
#     # return [c for c in res if c!='']
#
# for k in range(10):
#     chaine = next_tab(chaine)

# print(max([chaine.count(letter) for letter in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ']))
# print(min([chaine.count(letter) for letter in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' if chaine.count(letter) != 0]))


def update(tab):
    p = {}
    for i in occu.keys():
        for j in occu.keys():
            p[i + j] = 0
    for k in tab.keys():
        if k in rules.keys():
            p[k[0] + rules[k]] += tab[k]
            p[rules[k] + k[1]] += tab[k]
            p[k] -= tab[k]
            occu[rules[k]] += tab[k]

    for k in tab.keys():
        tab[k] += p[k]
    return tab


for _ in range(40):
    pairs = update(pairs)
print(pairs)
print(occu)
print(max(occu.values())-min(occu.values()))