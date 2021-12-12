inp = open("AOC_txt_10").read().split("\n")
ch = []
broken = []
def mirror(brack):
    if brack=='(': return ')'
    elif brack=='[': return ']'
    elif brack=='{': return '}'
    else: return '>'

for k in range(len(inp)):
    line=inp[k]
    stack = []
    for obj in line:
        if obj in '([{<':
            stack.append(obj)
        else:
            if obj != mirror(stack.pop()):
                ch.append(obj)
                broken.append(k)
                break

# print(3*ch.count(')') + 57*ch.count(']') + 1197*ch.count('}') + 25137*ch.count('>'))

new_inp = [inp[k] for k in range(len(inp)) if k not in broken]
print(new_inp)
completion = []

for k in range(len(new_inp)):
    line=new_inp[k]
    stack = []
    for obj in line:
        if obj in '([{<':
            stack.append(obj)
        else:
            stack.pop()
    completion.append(''.join([mirror(char) for char in stack][::-1]))

print(completion)

cscore = lambda k:'*)]}>'.index(k)
def score(line):
    s = 0
    for k in line:
        s = 5*s + cscore(k)
    return s

scoref = sorted([score(line) for line in completion])
print(scoref)
print(scoref[len(scoref)//2])