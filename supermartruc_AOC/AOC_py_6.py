fish = [[int(j) for j in open("AOC_txt_6").read().split(',')].count(k) for k in range(9)]
n = 256  # n = 80
for i in range(n):
    fish = [fish[(k + 1) % 9] + (k == 6) * fish[0] for k in range(len(fish))]
print(sum(fish))