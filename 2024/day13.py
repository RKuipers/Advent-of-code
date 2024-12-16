import math
import time
from functools import cache
from utils import Coord

MAX_VAL = 2**31
ADDED_VAL = 10000000000000


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    machines = []
    for i in range(int((len(lines) + 1) / 4)):
        m = lines[i * 4 : (i + 1) * 4]
        ax = int(m[0].strip()[m[0].find("X+") + 2 : m[0].find("Y+") - 2])
        ay = int(m[0].strip()[m[0].find("Y+") + 2 :])
        bx = int(m[1].strip()[m[1].find("X+") + 2 : m[1].find("Y+") - 2])
        by = int(m[1].strip()[m[1].find("Y+") + 2 :])
        gx = int(m[2].strip()[m[2].find("X=") + 2 : m[2].find("Y=") - 2])
        gy = int(m[2].strip()[m[2].find("Y=") + 2 :])
        machines.append((Coord(ax, ay), Coord(bx, by), Coord(gx, gy)))

    return machines



def parseB(file):
    parsed = parseA(file)
    return [(m[0], m[1], Coord(m[2].x + ADDED_VAL, m[2].y + ADDED_VAL)) for m in parsed]


@cache
def count_machines(a, b, g, ad, bd):
    if g == a:
        return 3
    elif g == b:
        return 1

    if not g >= Coord(0, 0) or (ad <= 0 and bd <= 0):
        return MAX_VAL

    a_opt = MAX_VAL
    b_opt = MAX_VAL

    if ad > 0:
        a_opt = 3 + count_machines(a, b, g - a, ad - 1, bd)
    if bd > 0:
        b_opt = 1 + count_machines(a, b, g - b, ad, bd - 1)

    return min(a_opt, b_opt)


def d13a(machines):
    vals = {}
    for i, m in enumerate(machines):
        v = count_machines(m[0], m[1], m[2], 100, 100)
        if v < MAX_VAL:
            vals[i] = v

    return sum(vals.values())


def d13b(machines):
    vals = {}
    for i, m in enumerate(machines):
        b = (m[2].x * m[0].y - m[2].y * m[0].x) // (m[0].y * m[1].x - m[1].y * m[0].x)
        a = (m[2].x * m[1].y - m[2].y * m[1].x) // (m[1].y * m[0].x - m[1].x * m[0].y)
        if m[0].x * a + m[1].x * b == m[2].x and m[0].y * a + m[1].y * b == m[2].y:
            vals[i] = 3 * a + b

    return sum(vals.values())


testfile = "2024/inputs/day13testinput.txt"
file = "2024/inputs/day13input.txt"

print("TEST:")
start = time.time()
print(d13b(parseA(testfile)))
mid = time.time()
print(d13b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d13b(parseA(file)))
mid = time.time()
print(d13b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")


'''
ax*a+bx*b=gx                            Get x to the goal
ay*a+by*b=gy                            Get y to the goal

a = (gx-bx*b) / ax                      Rearrange x equation for a
a = (gy-by*b) / ay                      Rearrange y equation for a
(gx-bx*b) / ax = (gy-by*b) / ay         Combine both a functions    
(gx-bx*b) * ay = (gy-by*b) * ax         Multiply by (ax * ay)
gx*ay - bx*b*ay = gy*ax - by*b*ax       Resolve brackets
gx*ay - gy*ax = bx*b*ay - by*b*ax       Add (bx*b*ay - gy*ax)
gx*ay - gy*ax = b * (bx*ay - by*ax)     Factor out b
(gx*ay - gy*ax) / (bx*ay - by*ax) = b   Divide by (bx*ay - by*ax)

(gx*by - gy*bx) / (ax*by - ay*bx) = a   Swap all a-terms for b and vice versa
'''
