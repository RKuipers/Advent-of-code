import time
from utils import Coord


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    keys = []
    locks = []

    fig = [0] * 5
    is_key = None
    for l in lines:
        if l.strip() == "":
            if is_key:
                keys.append(fig)
            else:
                locks.append([c - 1 for c in fig])

            fig = [0] * 5
            is_key = None
            continue
        elif is_key is None:
            is_key = "#" in l
            continue
        for i, c in enumerate(l):
            if c == "#":
                fig[i] += 1
    if is_key:
        keys.append(fig)
    else:
        locks.append([c - 1 for c in fig])
    
    return keys, locks


def parseB(file):
    return parseA(file)


def d25a(parsed):
    keys, locks = parsed
    return [all([k + l <= 5 for k, l in zip(k, l)]) for k in keys for l in locks].count(
        True
    )


def d25b(parsed):
    print("Merry Christmas!")


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
