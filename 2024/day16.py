from heapq import heappop, heappush
import time
from utils import Coord, a_star

right = {
    Coord(1, 0): Coord(0, 1),
    Coord(0, 1): Coord(-1, 0),
    Coord(-1, 0): Coord(0, -1),
    Coord(0, -1): Coord(1, 0),
}
left = {
    Coord(1, 0): Coord(0, -1),
    Coord(0, -1): Coord(-1, 0),
    Coord(-1, 0): Coord(0, 1),
    Coord(0, 1): Coord(1, 0),
}

MAXVAL = 2**31


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    size = Coord(len(lines[-1]), len(lines))
    walls = [[] for _ in range(size.y)]
    for y in range(size.y):
        for x in range(size.x):
            if lines[y][x] == "S":
                start = Coord(x, y)
            elif lines[y][x] == "E":
                end = Coord(x, y)
            elif lines[y][x] == "#":
                walls[y].append(x)

    return start, end, walls


def parseB(file):
    return parseA(file)


def heuristic(v, cg):
    cv, dv = v
    dis_x = abs(cv.x - cg.x)
    dis_y = abs(cv.y - cg.y)
    turning = 0
    if dis_x > 0 and dis_y > 0:
        turning += 1000
    if (
        (dv.x > 0 and cv.x >= cg.x)
        or (dv.x < 0 and cv.x <= cg.x)
        or (dv.y > 0 and cv.y >= cg.y)
        or (dv.y < 0 and cv.y <= cg.y)
    ):
        turning += 1000
    return turning + dis_x + dis_y


def reconstruct(cameFrom, current):
    total_path = [current]
    currents = [current]
    while len(currents) > 0:
        current = currents.pop()
        if current in cameFrom.keys():
            currents += cameFrom[current]
            total_path += cameFrom[current]
    return total_path


def a_star_local(start, goal, walls):
    openset = []
    heappush(openset, (0, start))
    cameFrom = {}
    gScore = {start: 0}

    while len(openset) > 0:
        curr_fScore, current = heappop(openset)
        if current[0] == goal:
            return gScore[current], reconstruct(cameFrom, current)
        curr_coord, curr_dir = current
        neighbors = [
            (1000, (curr_coord, left[curr_dir])),
            (1000, (curr_coord, right[curr_dir])),
        ]
        next_coord = curr_coord + curr_dir
        if not next_coord.x in walls[next_coord.y]:
            neighbors.append((1, (curr_coord + curr_dir, curr_dir)))
        for d, n in neighbors:
            tentative_gScore = gScore[current] + d
            if gScore.get(n, MAXVAL) > tentative_gScore:
                cameFrom[n] = [current]
                gScore[n] = tentative_gScore
                if not n in {x[1] for x in openset}:
                    heappush(openset, (tentative_gScore + heuristic(n, goal), n))
            elif gScore.get(n, MAXVAL) == tentative_gScore:
                cameFrom[n].append(current)

    return MAXVAL


def get_neighbors(current, walls):
    curr_coord, curr_dir = current
    neighbors = [
        (1000, (curr_coord, left[curr_dir])),
        (1000, (curr_coord, right[curr_dir])),
    ]
    next_coord = curr_coord + curr_dir
    if not next_coord.x in walls[next_coord.y]:
        neighbors.append((1, (curr_coord + curr_dir, curr_dir)))
    return neighbors


def d16a(parsed):
    start, end, walls = parsed
    dir = Coord(1, 0)
    score, path = a_star(
        (start, dir),
        end,
        heuristic,
        get_neighbors,
        walls,
    )
    return score, path


def d16b(parsed, a_result):
    path_nodes = {step[0] for step in a_result[1]}
    return len(path_nodes)


testfile = "2024/inputs/day16testinput.txt"
file = "2024/inputs/day16input.txt"

print("TEST:")
start = time.time()
a_result = d16a(parseA(testfile))
print(a_result[0])
mid = time.time()
print(d16b(parseB(testfile), a_result))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
a_result = d16a(parseA(file))
print(a_result[0])
mid = time.time()
print(d16b(parseB(file), a_result))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
