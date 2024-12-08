import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d23a(parsed):
    pass


def d23b(parsed):
    pass


testfile = "2024/inputs/day23testinput.txt"
file = "2024/inputs/day23input.txt"

print("TEST:")
start = time.time()
print(d23a(parseA(testfile)))
mid = time.time()
print(d23b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d23a(parseA(file)))
mid = time.time()
print(d23b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
