import time

def parse(file):
    with open(file) as f:
        lines = f.readlines()

    return [[int(lev) for lev in rep.split()] for rep in lines]

def safe(rep):
    diffs = [rep[i+1] - rep[i]for i in range(len(rep) - 1)]
    return all([x in [1, 2, 3] for x in diffs]) or all([x in [-1, -2, -3] for x in diffs])

def d2a(file):
    reports = parse(file)

    return sum([1 for rep in reports if safe(rep)])

def d2b(file):
    reports = parse(file)

    count = 0
    for rep in reports:
        for i in range(len(rep)):
            r = rep[:i] + rep[(i+1):]
            if safe(r):
                count += 1
                break

    return count

file = "2024/inputs/day2input.txt"
start = time.time()
print(d2a(file))
mid = time.time()
print(d2b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
