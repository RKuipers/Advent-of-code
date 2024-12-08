import time
from utils import Coord

def add(c1, c2):
    return (c1[0] + c2[0], c1[1] + c2[1])

def subtract(c1, c2):
    return (c1[0] - c2[0], c1[1] - c2[1])

def mult(c, s):
    return (c[0] * s, c[1] * s)

def in_bound(c, size):
    return c[0] >= 0 and c[1] >= 0 and c[0] < size[0] and c[1] < size[1]

def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    size = Coord(len(lines[-1]), len(lines))
    ants = {}
    for x in range(size.x):
        for y in range(size.y):
            if lines[y][x] != ".":
                if ants.get(lines[y][x]) is None:
                    ants[lines[y][x]] = []
                ants[lines[y][x]].append(Coord(x, y))

    return size, ants

def parseB(file):
    return parseA(file)

def d8a(parsed):
    res = set()
    for k, v in parsed[1].items():
        for i, v1 in enumerate(v):
            for j in range(i+1, len(v)):
                v2 = v[j]
                d = v1 - v2
                p1 = v1 + d
                p2 = v2 - d
                if p1 < parsed[0] and p1 >= Coord(0, 0):
                    res.add(p1)
                if p2 < parsed[0] and p2 >= Coord(0, 0):
                    res.add(p2)

    return len(res)

def d8b(parsed):
    res = set()
    for k, v in parsed[1].items():
        for i, v1 in enumerate(v):
            for j in range(i+1, len(v)):
                v2 = v[j]
                res.add(v1)
                res.add(v2)
                d = v1 - v2
                p1 = v1 + d
                p2 = v2 - d
                while p1 < parsed[0] and p1 >= Coord(0, 0):
                    res.add(p1)
                    p1 = p1 + d
                while p2 < parsed[0] and p2 >= Coord(0, 0):
                    res.add(p2)
                    p2 = p2 - d

    return len(res)

testfile = "2024/inputs/day8testinput.txt"
file = "2024/inputs/day8input.txt"

print("TEST:")
start = time.time()
print(d8a(parseA(testfile)))
mid = time.time()
print(d8b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d8a(parseA(file)))
mid = time.time()
print(d8b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")