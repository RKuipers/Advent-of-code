import time
import re

def parseA(file):
    with open(file) as f:
        matches = re.findall("mul\([0-9]{1,3},[0-9]{1,3}\)", f.read())

    vals = []
    for m in matches:
        [x, y] = m.removeprefix('mul(')[:-1].split(",")
        vals.append(int(x) * int(y))

    return vals

def parseB(file):
    with open(file) as f:
        matches = re.findall("mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don't\(\)", f.read())

    vals = []
    do = True
    for m in matches:
        if m == 'do()':
            do = True
            continue
        elif m == "don't()":
            do = False
            continue
        if do:
            [x, y] = m.removeprefix('mul(')[:-1].split(",")
            vals.append(int(x) * int(y))

    return vals

def d3a(parsed):
    return sum(parsed)

def d3b(parsed):
    return sum(parsed)

testfile = "2024/inputs/day3testinput.txt"
file = "2024/inputs/day3input.txt"

print("TEST:")
start = time.time()
print(d3a(parseA(testfile)))
mid = time.time()
print(d3b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d3a(parseA(file)))
mid = time.time()
print(d3b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")