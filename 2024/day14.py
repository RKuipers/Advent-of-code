import time
from utils import Coord


def parseA(file):
    pass


def parseB(file):
    return parseA(file)


def d14a(parsed):
    pass


def d14b(parsed):
    pass


testfile = "2024/inputs/day14testinput.txt"
file = "2024/inputs/day14input.txt"

print("TEST:")
start = time.time()
print(d14a(parseA(testfile)))
mid = time.time()
print(d14b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d14a(parseA(file)))
mid = time.time()
print(d14b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
