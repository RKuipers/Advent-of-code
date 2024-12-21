from functools import cache
import random
import time
from utils import Coord, MAXVAL

number_keypad = {
    7: Coord(0, 0),
    8: Coord(1, 0),
    9: Coord(2, 0),
    4: Coord(0, 1),
    5: Coord(1, 1),
    6: Coord(2, 1),
    1: Coord(0, 2),
    2: Coord(1, 2),
    3: Coord(2, 2),
    0: Coord(1, 3),
    -1: Coord(2, 3),
}

direction_keypad = {
    Coord(0, -1): Coord(1, 0),
    -1: Coord(2, 0),
    Coord(-1, 0): Coord(0, 1),
    Coord(0, 1): Coord(1, 1),
    Coord(1, 0): Coord(2, 1),
}

stringify_direction_keypad = {
    Coord(0, -1): "^",
    -1: "A",
    Coord(-1, 0): "<",
    Coord(0, 1): "v",
    Coord(1, 0): ">",
}


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    return [list(map(int, l.strip()[:-1])) + [-1] for l in lines]


def parseB(file):
    return parseA(file)


@cache
def get_keys(start, end, deadspace):
    if start == end:
        return [-1]

    m = end - start

    if m.y == 0:
        return abs(m.x) * [Coord(m.x // abs(m.x), 0)] + [-1]
    elif m.x == 0:
        return abs(m.y) * [Coord(0, m.y // abs(m.y))] + [-1]
    y_first = random.random() < 0.5
    if Coord(start.x + m.x, start.y) == deadspace:
        y_first = True
    elif Coord(start.x, start.y + m.y) == deadspace:
        y_first = False
    if y_first:
        return (
            abs(m.y) * [Coord(0, m.y // abs(m.y))]
            + abs(m.x) * [Coord(m.x // abs(m.x), 0)]
            + [-1]
        )
    else:
        return (
            abs(m.x) * [Coord(m.x // abs(m.x), 0)]
            + abs(m.y) * [Coord(0, m.y // abs(m.y))]
            + [-1]
        )


def solve(code, depth):
    min_path = MAXVAL
    for _ in range(10):
        get_keys.cache_clear()
        path = []

        prev = -1
        for i, c in enumerate(code):
            path += get_keys(number_keypad[prev], number_keypad[c], Coord(0, 3))
            prev = c

        for iteration in range(depth):
            new_path = []
            prev = -1
            for i, c in enumerate(path):
                new_path += get_keys(
                    direction_keypad[prev],
                    direction_keypad[c],
                    Coord(0, 0),
                )
                prev = c
            path = new_path
        if len(path) < min_path:
            min_path = len(path)
    return min_path


def d21a(parsed):
    complexities = []
    for code in parsed:
        min_path = solve(code, 2)
        complexities.append(int("".join(list(map(str, code[:-1])))) * min_path)
    return sum(complexities)


def d21b(parsed):
    complexities = []
    for code in parsed:
        min_path = solve(code, 25)
        complexities.append(int("".join(list(map(str, code[:-1])))) * min_path)
    return sum(complexities)


testfile = "2024/inputs/day21testinput.txt"
file = "2024/inputs/day21input.txt"

print("TEST:")
start = time.time()
print(d21a(parseA(testfile)))
mid = time.time()
print(d21b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d21a(parseA(file)))
mid = time.time()
print(d21b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
