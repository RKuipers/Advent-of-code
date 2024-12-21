from functools import cache
import random
import time

number_keypad = {
    "7": 0,
    "8": 1,
    "9": 2,
    "4": 1j,
    "5": 1 + 1j,
    "6": 2 + 1j,
    "1": 2j,
    "2": 1 + 2j,
    "3": 2 + 2j,
    " ": 3j,
    "0": 1 + 3j,
    "A": 2 + 3j,
}

direction_keypad = {
    " ": 0,
    "^": 1,
    "A": 2,
    "<": 1j,
    "v": 1 + 1j,
    ">": 2 + 1j,
}


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    return [l.strip() for l in lines]


def parseB(file):
    return parseA(file)


@cache
def get_keys(start, end):
    pad = (
        number_keypad
        if (start in number_keypad and end in number_keypad)
        else direction_keypad
    )

    if start == end:
        return "A"

    m = pad[end] - pad[start]

    if m.imag == 0:
        return int(-m.real) * "<" + int(m.real) * ">" + "A"
    elif m.real == 0:
        return int(-m.imag) * "^" + int(m.imag) * "v" + "A"

    y_first = random.random() < 0.5
    if pad[start] + m.real == pad[" "]:
        y_first = True
    elif pad[start] + m.imag * 1j == pad[" "]:
        y_first = False

    if y_first:
        return (
            int(-m.imag) * "^"
            + int(m.imag) * "v"
            + int(-m.real) * "<"
            + int(m.real) * ">"
            + "A"
        )
    else:
        return (
            int(-m.real) * "<"
            + int(m.real) * ">"
            + int(-m.imag) * "^"
            + int(m.imag) * "v"
            + "A"
        )


@cache
def get_path(code, depth):
    v = 0
    if depth == 0:
        return len(code)
    for i, c in enumerate(code):
        v += get_path(get_keys(code[i - 1], c), depth - 1)
    return v


def solve(code, depth):
    min_path = -1
    for _ in range(1000):
        get_keys.cache_clear()
        get_path.cache_clear()
        path = get_path(code, depth)
        if path < min_path or min_path == -1:
            min_path = path
    return min_path


def d21a(parsed):
    complexities = []
    for code in parsed:
        min_path = solve(code, 3)
        complexities.append(int("".join(list(map(str, code[:-1])))) * min_path)
    return sum(complexities)


def d21b(parsed):
    complexities = []
    for code in parsed:
        min_path = solve(code, 26)
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
