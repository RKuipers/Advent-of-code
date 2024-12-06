import time


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    rules = []
    updates = []
    for l in lines:
        if "|" in l:
            s = l.split("|")
            rules.append((int(s[0]), int(s[1])))
        elif "," in l:
            updates.append([int(x) for x in l.split(",")])

    return rules, updates


def parseB(file):
    return parseA(file)


def check_update(update, rules):
    for r in rules:
        if r[0] in update and r[1] in update:
            i0 = update.index(r[0])
            i1 = update.index(r[1])
            if i1 < i0:
                return False, (i1, i0)
    return True, (None, None)


def d5a(parsed):
    valids = []
    for update in parsed[1]:
        if check_update(update, parsed[0])[0]:
            valids.append(update)

    return sum([update[int(len(update) / 2)] for update in valids])


def d5b(parsed):
    valids = []
    for update in parsed[1]:
        val, (i1, i0) = check_update(update, parsed[0])
        if val:
            continue
        val = False
        while not val:
            fixed_update = (
                update[:i1]
                + [update[i0]]
                + update[i1 + 1 : i0]
                + [update[i1]]
                + update[i0 + 1 :]
            )
            val, (i1, i0) = check_update(fixed_update, parsed[0])
            update = fixed_update
        valids.append(fixed_update)
    return sum([update[int(len(update) / 2)] for update in valids])


testfile = "2024/inputs/day5testinput.txt"
file = "2024/inputs/day5input.txt"

print("TEST:")
start = time.time()
print(d5a(parseA(testfile)))
mid = time.time()
print(d5b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d5a(parseA(file)))
mid = time.time()
print(d5b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
