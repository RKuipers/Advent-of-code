import time


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    return [[int(lev) for lev in rep.split()] for rep in lines]


def parseB(file):
    return parseA(file)


def safe(rep):
    diffs = [rep[i + 1] - rep[i] for i in range(len(rep) - 1)]
    return all([x in [1, 2, 3] for x in diffs]) or all(
        [x in [-1, -2, -3] for x in diffs]
    )


def d2a(parsed):
    return sum([1 for rep in parsed if safe(rep)])


def d2b(parsed):
    count = 0
    for rep in parsed:
        for i in range(len(rep)):
            r = rep[:i] + rep[(i + 1) :]
            if safe(r):
                count += 1
                break

    return count


testfile = "2024/inputs/day2testinput.txt"
file = "2024/inputs/day2input.txt"

print("TEST:")
start = time.time()
print(d2a(parseA(testfile)))
mid = time.time()
print(d2b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d2a(parseA(file)))
mid = time.time()
print(d2b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
