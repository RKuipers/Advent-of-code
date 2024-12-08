import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d20a(parsed):
    pass


def d20b(parsed):
    pass


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
