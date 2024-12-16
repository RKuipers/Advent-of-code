import time
import math
import sys
import functools
from utils import Coord

sys.setrecursionlimit(50000)


def parseA(file):
    with open(file) as f:
        line = f.readlines()[0]

    numbers = line.split()
    return [int(x) for x in numbers]


def parseB(file):
    return parseA(file)


def blink(s):
    if s == 0:
        return [1]
    n_digits = math.floor(math.log10(s)) + 1
    if n_digits % 2 == 0:
        mult = 10 ** (n_digits / 2)
        left = int(s / mult)
        right = int(s - left * mult)
        return [left, right]
    return [2024 * s]


def d11a(parsed):
    numbers = parsed
    for _ in range(25):
        next = map(blink, numbers)
        numbers = [x for n in next for x in n]

    return len(numbers)


def count_resulting_stones(val, blinks, memory):
    if memory.get(val) is None:
        memory[val] = {0: 1}
    mem = memory[val].get(blinks)
    if not mem is None:
        return mem, memory

    vals = blink(val)
    res = 0
    for v in vals:
        rec, memory = count_resulting_stones(v, blinks - 1, memory)
        res += rec

    memory[val][blinks] = res
    return res, memory


def d11b(parsed):
    memory = {v: {0: 1} for v in parsed}

    result = 0
    for v in parsed:
        res, memory = count_resulting_stones(v, 75, memory)
        result += res

    return result


testfile = "2024/inputs/day11testinput.txt"
file = "2024/inputs/day11input.txt"

print("TEST:")
start = time.time()
print(d11a(parseA(testfile)))
mid = time.time()
print(d11b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d11a(parseA(file)))
mid = time.time()
print(d11b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
