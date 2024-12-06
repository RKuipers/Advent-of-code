import time


def parseA(file):
    with open(file) as f:
        lines = f.readlines()
    return lines


def parseB(file):
    return parseA(file)


def d4a(parsed):
    small1 = "XMAS"
    small2 = "SAMX"
    count = 0
    for big in parsed:
        count += big.count(small1)
        count += big.count(small2)

    pass

    transposed = list(map(list, zip(*parsed)))

    for big in transposed:
        count += "".join(big).count(small1)
        count += "".join(big).count(small2)

    pass

    diag1 = [[parsed[i - j][j] for j in range(i + 1)] for i in range(len(parsed))] + [
        [
            parsed[j][i - j + len(parsed[-1]) - 1]
            for j in range(len(parsed[-1]) - 1, i - 1, -1)
        ]
        for i in range(1, len(parsed[-1]))
    ]
    diag2 = [
        [parsed[i + j][j] for j in range(len(parsed)) if i + j < len(parsed)]
        for i in range(len(parsed))
    ] + [
        [parsed[j][i + j] for j in range(len(parsed[0])) if i + j < len(parsed[-1])]
        for i in range(1, len(parsed[0]))
    ]

    for big in diag1:
        count += "".join(big).count(small1)
        count += "".join(big).count(small2)

    pass
    for big in diag2:
        count += "".join(big).count(small1)
        count += "".join(big).count(small2)

    pass

    return count


def d4b(parsed):
    count = 0
    for y in range(1, len(parsed) - 1):
        for x in range(1, len(parsed[y]) - 1):
            if parsed[y][x] == "A":
                tl = parsed[y - 1][x - 1]
                tr = parsed[y - 1][x + 1]
                bl = parsed[y + 1][x - 1]
                br = parsed[y + 1][x + 1]
                if (tr == "M" and bl == "S") or (tr == "S" and bl == "M"):
                    if (tl == "M" and br == "S") or (tl == "S" and br == "M"):
                        count += 1

    return count


testfile = "2024/inputs/day4testinput.txt"
file = "2024/inputs/day4input.txt"

print("TEST:")
start = time.time()
print(d4a(parseA(testfile)))
mid = time.time()
print(d4b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d4a(parseA(file)))
mid = time.time()
print(d4b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
