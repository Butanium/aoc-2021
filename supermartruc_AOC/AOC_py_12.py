vertices = [sorted(line.split('-')) for line in open("AOC_txt_12").read().split("\n")]
nodes = []
chemins1 = []
chemins2 = []


def is_small(s):
    return s == s.lower()


for v in vertices:
    if v[0] not in nodes + ['start', 'end']:
        nodes.append(v[0])
    if v[1] not in nodes + ['start', 'end']:
        nodes.append(v[1])
nodes = ['start'] + nodes + ['end']  # Initialisation nodes

voisins = [[vois for vois in nodes if sorted([node, vois]) in vertices] for node in nodes]


def cherche_chemin1(node, liste):
    if node == 'end':
        chemins1.append(liste + ['end'])
    else:
        for voisin in voisins[nodes.index(node)]:
            if not (voisin in liste and is_small(voisin)):
                cherche_chemin1(voisin, liste + [node])


def cherche_chemin2(node, liste, visited):
    if node == 'end':
        chemins2.append(liste + ['end'])
    else:
        for voisin in voisins[nodes.index(node)]:
            if voisin != 'start':
                if not (is_small(voisin) and voisin in liste):
                    cherche_chemin2(voisin, liste + [node], visited)
                elif not visited:
                    cherche_chemin2(voisin, liste + [node], True)


cherche_chemin1('start', [])
cherche_chemin2('start', [], False)

print(len(chemins1))
print(len(chemins2))
