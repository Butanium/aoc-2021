hinput = open("AOC_txt_16").read()

def hex_to_bin(s):
    res = ''
    for ch in s:
        h = bin(int(ch, 16))[2:]
        res += (4 - len(h)) * '0' + h
    return res

def get_version(packet): return int(packet[:3], 2)

def get_id(packet): return int(packet[3:6], 2)

def get_l_type_id(operator): return int(operator[6])

def build_tree(chaine):
    if get_id(chaine) == 4:
        pointeur = 6
        while chaine[pointeur] == '1':
            pointeur += 5
        pointeur += 5
        return chaine[:pointeur], pointeur, []

    sub_liste = []
    if get_l_type_id(chaine) == 0:
        bit_length = int(chaine[7:22], 2)
        sum_length = sum([k[1] for k in sub_liste])
        while sum_length != bit_length:
            sub_liste.append(build_tree(chaine[22 + sum_length:]))
            sum_length += sub_liste[-1][1]
        return chaine[:22 + sum_length], 22 + sum_length, sub_liste

    else:
        nb_sub = int(chaine[7:18], 2)
        sum_length = sum([k[1] for k in sub_liste])
        while len(sub_liste) != nb_sub:
            sub_liste.append(build_tree(chaine[18 + sum_length:]))
            sum_length += sub_liste[-1][1]
        return chaine[:18 + sum_length], 18 + sum_length, sub_liste


def get_sum_version(tree):
    packet, length, sub_liste = tree
    return get_version(packet) + sum([get_sum_version(item) for item in sub_liste])


def ev(packet):
    res = ''
    pointeur = 6
    while packet[pointeur] == '1':
        res += packet[pointeur + 1:pointeur + 5]
        pointeur += 5
    res += packet[pointeur + 1:pointeur + 5]
    return int(res, 2)


def evaluate(tree):
    packet, length, sub_list = tree
    pid = get_id(packet)
    e_liste = [evaluate(item) for item in sub_list]
    if pid == 0: return sum(e_liste)
    if pid == 1:
        p = 1
        for k in e_liste:
            p *= k
        return p
    if pid == 2: return min(e_liste)
    if pid == 3: return max(e_liste)
    if pid == 4: return ev(packet)
    if pid == 5: return int(e_liste[0] > e_liste[1])
    if pid == 6: return int(e_liste[0] < e_liste[1])
    if pid == 7: return int(e_liste[0] == e_liste[1])


# print(get_sum_version(build_tree(hex_to_bin(hinput))))    #Part 1
# print(evaluate(build_tree(hex_to_bin(hinput))))           #Part 2
