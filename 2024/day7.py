import time


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    splits = [l.split(" ") for l in lines]
    return [(int(l[0][:-1]), [int(x) for x in l[1:]]) for l in splits]


def parseB(file):
    return parseA(file)


def checkA(final, vals, partial):
    if len(vals) == 0:
        return final == partial
    if partial > final:
        return False
    return checkA(final, vals[1:], vals[0] + partial) or checkA(
        final, vals[1:], vals[0] * partial
    )


def cc(x, y):
    return int(str(x) + str(y))


def checkB(final, vals, partial):
    if len(vals) == 0:
        return final == partial
    if partial > final:
        return False
    return (
        checkB(final, vals[1:], vals[0] + partial)
        or checkB(final, vals[1:], vals[0] * partial)
        or checkB(final, vals[1:], cc(partial, vals[0]))
    )


def d7a(parsed):
    return sum([k for (k, v) in parsed if checkA(k, v[1:], v[0])])


def d7b(parsed):
    return sum([k for (k, v) in parsed if checkB(k, v[1:], v[0])])


testfile = "2024/inputs/day7testinput.txt"
file = "2024/inputs/day7input.txt"

print("TEST:")
start = time.time()
print(d7a(parseA(testfile)))
mid = time.time()
print(d7b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d7a(parseA(file)))
mid = time.time()
print(d7b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
