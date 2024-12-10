import time
from utils import Coord


def parseA(file):
    with open(file) as f:
        line = f.readlines()[0]

    if len(line) % 2 == 1:
        line = line + "0"

    return [(int(line[i]), int(line[i + 1])) for i in range(0, len(line), 2)]


def parseB(file):
    with open(file) as f:
        line = f.readlines()[0]

    if len(line) % 2 == 1:
        line = line + "0"

    return {
        int(i / 2): (int(line[i]), int(line[i + 1]), int(i / 2))
        for i in range(0, len(line), 2)
    }


def d9a(parsed):
    total_file1 = sum([p[0] for p in parsed])
    total_empty1 = sum([p[1] for p in parsed])

    numbers = []
    backlook = -1
    for i in range(len(parsed)):
        (f, e) = parsed[i]
        if f == 0:
            break
        numbers += [i] * f
        if len(parsed) + backlook <= i:
            break
        while e > 0:
            swapped = min(e, parsed[backlook][0])
            parsed[backlook] = (
                parsed[backlook][0] - swapped,
                parsed[backlook][1] + swapped,
            )
            numbers += [len(parsed) + backlook] * swapped
            e -= swapped
            if parsed[backlook][0] == 0:
                backlook -= 1
                if len(parsed) + backlook <= i:
                    break

    total_file2 = sum([p[0] for p in parsed])
    total_empty2 = sum([p[1] for p in parsed])

    assert total_empty1 + total_file1 == total_empty2 + total_file2
    assert len(numbers) == total_file1
    assert max(numbers) == len(parsed) - 1

    return sum([i * x for i, x in enumerate(numbers)])


def d9b(parsed):
    numbers = list(parsed.values())
    indices = list(range(len(parsed)))

    for j in sorted(list(range(len(parsed))), reverse=True):
        (f_, e_, id_) = parsed[j]
        index_j = indices[id_]
        (f_, e_, id_) = numbers[index_j]
        for i in range(0, index_j):
            (f, e, id) = numbers[i]
            if e >= f_:
                numbers[i] = (f, 0, id)
                if i + 1 == index_j:
                    numbers[index_j] = (f_, e_ + e, id_)
                else:
                    numbers[index_j - 1] = (
                        numbers[index_j - 1][0],
                        numbers[index_j - 1][1] + f_ + e_,
                        numbers[index_j - 1][2],
                    )
                    numbers[index_j] = (f_, e - f_, id_)
                    numbers.insert(i + 1, numbers.pop(index_j))

                    for k in range(i + 1, index_j + 1):
                        indices[numbers[k][2]] += 1
                    indices[id_] = i + 1

                break

    numberline = []
    for f, e, id in numbers:
        numberline += [id] * f
        numberline += [None] * e

    return sum([i * x for i, x in enumerate(numberline) if not x is None])


testfile = "2024/inputs/day9testinput.txt"
file = "2024/inputs/day9input.txt"

print("TEST:")
start = time.time()
print(d9a(parseA(testfile)))
mid = time.time()
print(d9b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d9a(parseA(file)))
mid = time.time()
print(d9b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
