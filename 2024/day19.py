import time
from functools import cache
from utils import Coord


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    towels = lines[0].strip().split(', ')

    designs = [l.strip() for l in lines[2:]]

    return towels, designs


def parseB(file):
    return parseA(file)



def d19a(parsed):
    @cache
    def is_possible(design):
        if design == "":
            return True
        for t in towels:
            if design.startswith(t):
                if is_possible(design.removeprefix(t)):
                    return True
        return False
    towels, designs = parsed
    possible = [is_possible(d) for d in designs]
    return possible.count(True)


def d19b(parsed):
    @cache
    def count_possible(design):
        possible = 0
        if design == "":
            return 1
        for t in towels:
            if design.startswith(t):
                possible += count_possible(design.removeprefix(t))
        return possible
    towels, designs = parsed
    possible = [count_possible(d) for d in designs]
    return sum(possible)


testfile = "2024/inputs/day19testinput.txt"
file = "2024/inputs/day19input.txt"

print("TEST:")
start = time.time()
print(d19a(parseA(testfile)))
mid = time.time()
print(d19b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d19a(parseA(file)))
mid = time.time()
print(d19b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
