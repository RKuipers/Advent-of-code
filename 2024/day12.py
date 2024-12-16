import time
import sys
from functools import cache
from utils import Coord, BFS

DIRS = [Coord(-1, 0), Coord(0, -1), Coord(1, 0), Coord(0, 1)]
CORNERS = [
    (Coord(0, 1), Coord(1, 0), Coord(1, 1)),
    (Coord(0, -1), Coord(1, 0), Coord(1, -1)),
    (Coord(0, 1), Coord(-1, 0), Coord(-1, 1)),
    (Coord(0, -1), Coord(-1, 0), Coord(-1, -1)),
]


sys.setrecursionlimit(50000)


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    grid = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line.strip()):
            grid[Coord(x, y)] = c

    seen = set()
    regions = []

    NoneFunc = lambda *args: None
    size = Coord(len(lines[0].strip()), len(lines))
    neighbors = lambda v, g: [
        v + d
        for d in DIRS
        if v + d < size and v + d >= Coord(0, 0) and g[(v + d)] == g[v]
    ]
    for coord in grid.keys():
        if coord in seen:
            continue

        char = grid[coord]
        region = [coord]
        BFS(
            coord,
            grid,
            lambda x, g: not g[x] == char,
            NoneFunc,
            False,
            neighbors,
            lambda n, v: region.append(n),
        )
        seen = seen.union(set(region))
        regions.append(region)

    return grid, regions, size


def parseB(file):
    return parseA(file)


def d12a(parsed):
    grid, regions, size = parsed
    vals = []
    for reg in regions:
        area = len(reg)
        peri = 4 * area
        for c in reg:
            neighs = [
                c + d
                for d in DIRS
                if c + d < size and c + d >= Coord(0, 0) and grid[(c + d)] == grid[c]
            ]
            peri -= len(neighs)
        vals.append(peri * area)
    return sum(vals)


def d12b(parsed):
    def get_sides(r):
        sides = 0
        for coord in r:
            for corn in CORNERS:
                if (
                    corn[0] + coord in r
                    and corn[1] + coord in r
                    and not corn[2] + coord in r
                ) or (not corn[0] + coord in r and not corn[1] + coord in r):
                    sides += 1
        return sides

    grid, regions, size = parsed
    sides = []
    areas = []

    for i, reg in enumerate(regions):
        areas.append(len(reg))
        sides.append(get_sides(reg))

    return sum(s * a for s, a in zip(sides, areas))


testfile = "2024/inputs/day12testinput.txt"
file = "2024/inputs/day12input.txt"

print("TEST:")
start = time.time()
print(d12a(parseA(testfile)))
mid = time.time()
print(d12b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d12a(parseA(file)))
mid = time.time()
print(d12b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
