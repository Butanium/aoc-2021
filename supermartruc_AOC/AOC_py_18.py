numbers = open("AOC_txt_18").read().split("\n")


def add(a, b):
    return '[' + a + ',' + b + ']'


def magnitude(x):
    if type(x) == list:
        return 3 * magnitude(x[0]) + 2 * magnitude(x[1])
    else:
        return x


def str_to_snb(string):
    pile = []
    for elt in string:
        if elt.isdigit():
            pile.append(int(elt))
        elif elt == ']':
            b = pile.pop()
            a = pile.pop()
            pile.append([a, b])
    return pile[0]


def split(n):
    return n // 2, n // 2 + n % 2


def stack_to_str(stack):
    res = ''
    for k in range(len(stack)):
        res += str(stack[k])
        if k < len(stack) - 1:
            if stack[k + 1] != ']' and stack[k] != '[':
                res += ','
    return res


def reduce(s):
    stackp = []
    stack = []
    for k in range(len(s)):
        if s[k].isdigit():
            if s[k + 1].isdigit():
                stackp.append(int(s[k] + s[k + 1]))
            elif not s[k - 1].isdigit():
                stackp.append(int(s[k]))
        elif s[k] != ',':
            stackp.append(s[k])
    stackp.reverse()
    p = 0
    r = False
    i = -1
    while stackp:
        a = stackp.pop()
        i += 1
        if a == '[':
            p += 1
            stack.append(a)
        elif a == ']':
            if p <= 4:
                p -= 1
                stack.append(a)
            else:
                stackp.reverse()
                y = stack.pop()
                x = stack.pop()
                stack.pop()
                for k in range(i - 4, -1, -1):
                    if type(stack[k]) == int:
                        stack[k] += x
                        r = True
                        break
                for k in range(len(stackp)):
                    if type(stackp[k]) == int:
                        stackp[k] += y
                        r = True
                        break
                stack = stack + [0] + stackp
                break
        else:
            stack.append(a)
    if not r:
        for k in range(len(stack)):
            if type(stack[k]) == int:
                if stack[k] >= 10:
                    splited = split(stack[k])
                    stack[k] = '['
                    stack.insert(k+1,splited[0])
                    stack.insert(k+2,splited[1])
                    stack.insert(k+3,']')
                    r = True
                    break
    s = stack_to_str(stack)
    if r:
        return reduce(s)
    else:
        return s

# PART 1
# res = numbers[0]
# for number in numbers[1:]:
#     res = reduce(add(res,number))
#
# print(magnitude(str_to_snb(res)))


# PART 2
# m = 0
# for n1 in numbers:
#     for n2 in numbers:
#         if n1 != n2:
#             m = max(magnitude(str_to_snb(reduce(add(n1,n2)))),magnitude(str_to_snb(reduce(add(n2,n1)))),m)
# print(m)



