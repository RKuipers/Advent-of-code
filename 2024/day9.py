import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d9a(parsed):
    pass


def d9b(parsed):
    pass


testfile = "2024/inputs/day9testinput.txt"
file = "2024/inputs/day9input.txt"

print("TEST:")
start = time.time()
print(d9a(parseA(testfile)))
mid = time.time()
print(d9b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d9a(parseA(file)))
mid = time.time()
print(d9b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
