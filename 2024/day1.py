import time


def d1a(file):
    with open(file) as f:
        lines = f.readlines()

    llist = []
    rlist = []
    for l in lines:
        s = l.split()
        llist.append(int(s[0]))
        rlist.append(int(s[1]))

    llist.sort()
    rlist.sort()

    return sum([abs(l - r) for (l, r) in zip(llist, rlist)])


def d1b(file):
    with open(file) as f:
        lines = f.readlines()

    llist = []
    rlist = []
    for l in lines:
        s = l.split()
        llist.append(int(s[0]))
        rlist.append(int(s[1]))

    return sum([l * rlist.count(l) for l in llist])


file = "2024/inputs/day1input.txt"
start = time.time()
print(d1a(file))
mid = time.time()
print(d1b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
