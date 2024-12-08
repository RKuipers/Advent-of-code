import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d11a(parsed):
    pass


def d11b(parsed):
    pass


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
