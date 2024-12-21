import time
from utils import Coord, a_star

DIRS = [Coord(1, 0), Coord(-1, 0), Coord(0, -1), Coord(0, 1)]


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    walls = []
    for l in lines:
        splt = l.strip().split(",")
        walls.append(Coord(int(splt[0]), int(splt[1])))

    return walls


def parseB(file):
    return parseA(file)


def get_neighbors(n, walls):
    pot_neighbors = [n + d for d in DIRS]
    limit = Coord(7, 7) if len(walls) < 1024 else Coord(71, 71)
    return [
        x for x in pot_neighbors if x >= Coord(0, 0) and x < limit and not x in walls
    ]


def d18a(parsed, goal):
    wall_limit = 1024 if goal.x == 70 else 12
    score, path = a_star(
        Coord(0, 0), goal, lambda c: c == goal, lambda c, g: c.man_dis(g), get_neighbors, parsed[:wall_limit]
    )
    return len(path)


def d18b(parsed, goal):
    lb = 1024 if goal.x == 70 else 12
    ub = len(parsed)

    while ub > lb + 1:
        limit = lb + (ub - lb) // 2
        score, path = a_star(
            Coord(0, 0), goal, lambda c: c == goal, lambda c, g: c.man_dis(g), get_neighbors, parsed[:limit]
        )
        if len(path) == 0:
            ub = limit
        else:
            lb = limit 

    return parsed[lb]


testfile = "2024/inputs/day18testinput.txt"
file = "2024/inputs/day18input.txt"

print("TEST:")
start = time.time()
print(d18a(parseA(testfile), Coord(6, 6)))
mid = time.time()
print(d18b(parseB(testfile), Coord(6, 6)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d18a(parseA(file), Coord(70, 70)))
mid = time.time()
print(d18b(parseB(file), Coord(70, 70)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
