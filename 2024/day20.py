import time
from utils import Coord, a_star, DIRS


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    walls = []
    for y, l in enumerate(lines):
        for x, c in enumerate(l.strip()):
            if c == "#":
                walls.append(Coord(x, y))
            elif c == "S":
                start = Coord(x, y)
            elif c == "E":
                end = Coord(x, y)

    return start, end, walls


def parseB(file):
    return parseA(file)


def get_neighbors_normal(c, walls, cheats=None):
    pot_neighbors = [c + d for d in DIRS]
    if cheats == None:
        return [n for n in pot_neighbors if not n in walls]
    else:
        new_cheats = cheats
        if cheats == 1:
            new_cheats = 0
        return [(n, new_cheats) for n in pot_neighbors if not n in walls]


def get_neighbors_cheat(v, walls):
    if v[1] == 0:
        return []
    pot_neighbors = [v[0] + d for d in DIRS]
    return [(n, v[1] - 1) for n in pot_neighbors if n in walls]


def calc_dis(start, end, walls, nodes):
    distances = {0: start}
    dis = 0
    while len(nodes) > 0:
        v = distances[dis]
        neighbors = get_neighbors_normal(v, walls)
        for n in neighbors:
            if n in nodes:
                distances[dis + 1] = n
                nodes.remove(n)
                continue
        dis = dis + 1
    return distances, dis - 1


def d20a(parsed):
    start, end, walls = parsed
    limit = 100
    size = Coord(max(w.x for w in walls), max(w.y for w in walls))
    saves = {}

    distances, end_dis = calc_dis(
        start,
        end,
        walls,
        {
            Coord(x, y)
            for x in range(size.x)
            for y in range(size.y)
            if not Coord(x, y) in walls and not Coord(x, y) == start
        },
    )

    for p1, c1 in distances.items():
        for p2 in range(p1 + limit + 2, len(distances)):
            c2 = distances[p2]
            if c1.man_dis(c2) <= 2:
                path_length = p1 + c1.man_dis(c2) + (end_dis - p2)
                save = end_dis - path_length
                if save in saves.keys():
                    saves[save] += 1
                else:
                    saves[save] = 1

    return sum(v for k, v in saves.items() if k >= limit)


def d20b(parsed):
    start, end, walls = parsed
    limit = 100
    size = Coord(max(w.x for w in walls), max(w.y for w in walls))
    saves = {}

    distances, end_dis = calc_dis(
        start,
        end,
        walls,
        {
            Coord(x, y)
            for x in range(size.x)
            for y in range(size.y)
            if not Coord(x, y) in walls and not Coord(x, y) == start
        },
    )

    for p1, c1 in distances.items():
        for p2 in range(p1 + limit + 2, len(distances)):
            c2 = distances[p2]
            if c1.man_dis(c2) <= 20:
                path_length = p1 + c1.man_dis(c2) + (end_dis - p2)
                save = end_dis - path_length
                if save in saves.keys():
                    saves[save] += 1
                else:
                    saves[save] = 1

    return sum(v for k, v in saves.items() if k >= limit)


testfile = "2024/inputs/day20testinput.txt"
file = "2024/inputs/day20input.txt"

print("TEST:")
start = time.time()
print(d20a(parseA(testfile)))
mid = time.time()
print(d20b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d20a(parseA(file)))
mid = time.time()
print(d20b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
