import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d25a(parsed):
    pass


def d25b(parsed):
    pass


testfile = "2024/inputs/day25testinput.txt"
file = "2024/inputs/day25input.txt"

print("TEST:")
start = time.time()
print(d25a(parseA(testfile)))
mid = time.time()
print(d25b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d25a(parseA(file)))
mid = time.time()
print(d25b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
