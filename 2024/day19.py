import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d19a(parsed):
    pass


def d19b(parsed):
    pass


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
