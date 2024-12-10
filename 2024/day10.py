import time
from utils import Coord, BFS, DFS, Counter

DIRS = [Coord(0, 1), Coord(0, -1), Coord(1, 0), Coord(-1, 0)]


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    starts = []
    grid = []
    for y, line in enumerate(lines):
        line = line.strip()
        grid.append([])
        for x, point in enumerate(line):
            v = int(point)
            grid[-1].append(v)
            if v == 0:
                starts.append(Coord(x, y))

    return grid, starts


def parseB(file):
    return parseA(file)


def score_head(s, grid, origin, size):
    queue = [s]
    explored = [s]
    score = 0
    while len(queue) > 0:
        v = queue.pop(0)
        if grid[v.y][v.x] == 9:
            score += 1
            continue
        pot_neighbors = [v + d for d in DIRS]
        neighbors = [
            w
            for w in pot_neighbors
            if w < size
            and w >= origin
            and grid[w.y][w.x] == grid[v.y][v.x] + 1
            and not w in explored
            and not w in queue
        ]
        for n in neighbors:
            explored.append(n)
            queue.append(n)

    return score


def count_paths(v, grid, origin, size):
    if grid[v.y][v.x] == 9:
        return 1

    pot_neighbors = [v + d for d in DIRS]
    neighbors = [
        w
        for w in pot_neighbors
        if w < size and w >= origin and grid[w.y][w.x] == grid[v.y][v.x] + 1
    ]
    return sum(count_paths(n, grid, origin, size) for n in neighbors)


def d10a(parsed):
    grid, starts = parsed
    size = Coord(len(grid[0]), len(grid))
    origin = Coord(0, 0)
    scores = 0
    for s in starts:
        scores += score_head(s, grid, origin, size)

    return scores


def d10b(parsed):
    grid, starts = parsed
    size = Coord(len(grid[0]), len(grid))
    origin = Coord(0, 0)
    scores = 0
    for s in starts:
        scores += count_paths(s, grid, origin, size)

    return scores


def d10a_gen(parsed):
    grid, starts = parsed
    scores = Counter()

    lookup = lambda v, g: g[v.y][v.x]
    NoneFunc = lambda *args: None
    neighbors = lambda v, g: [
        v + d
        for d in DIRS
        if v + d < Coord(len(g[0]), len(g))
        and v + d >= Coord(0, 0)
        and g[(v + d).y][(v + d).x] == g[v.y][v.x] + 1
    ]
    for s in starts:
        BFS(
            s,
            grid,
            lambda x, g: lookup(x, g) == 9,
            lambda *args: scores.count_one(),
            False,
            neighbors,
            NoneFunc,
        )

    return scores.count


def d10b_gen(parsed):
    grid, starts = parsed
    scores = []

    lookup = lambda v, g: g[v.y][v.x]
    NoneFunc = lambda *args: None
    neighbors = lambda v, g: [
        v + d
        for d in DIRS
        if v + d < Coord(len(g[0]), len(g))
        and v + d >= Coord(0, 0)
        and g[(v + d).y][(v + d).x] == g[v.y][v.x] + 1
    ]

    for s in starts:
        scores.append(
            DFS(
                s,
                grid,
                lambda x, g: lookup(x, g) == 9,
                NoneFunc,
                neighbors,
                NoneFunc,
                lambda x, _: False,
            )
        )

    return sum(len(score) for score in scores)


testfile = "2024/inputs/day10testinput.txt"
file = "2024/inputs/day10input.txt"

print("TEST:")
start = time.time()
print(d10a(parseA(testfile)))
mid = time.time()
print(d10b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d10a(parseA(file)))
mid = time.time()
print(d10b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL GEN:")
start = time.time()
print(d10a_gen(parseA(file)))
mid = time.time()
print(d10b_gen(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
