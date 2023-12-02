import math


with open("inputs/2023/4.txt") as fd:
    txt = fd.read()


notEmpty = lambda x: len(x) != 0

scores = []
s      = 0

for line in txt.split("\n"):
    l = line.strip().rstrip()
    if len(l) == 0: continue
    cards = l.split(":")[-1].split("|")
    wins  = list(map(int, filter(notEmpty, cards[0].split(" "))))
    won   = list(map(int, filter(notEmpty, cards[1].split(" "))))
    p     = len(list(filter(lambda x: x in wins, won)))

    scores.append(p)

    if p != 0:
        s += int(math.pow(2, p - 1))

print(f"p1 = {s}")


counts = [1] * len(scores)

for i in range(len(scores)):
    if scores[i] == 0: continue
    for j in range(i + 1, min(len(scores), 1 + i + scores[i])):
        counts[j] += counts[i]

print(f"p2 = {sum(counts)}")
