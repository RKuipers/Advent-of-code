import multiprocessing.managers
import time
import multiprocessing

right = {(0, 1): (-1, 0), (-1, 0): (0, -1), (0, -1): (1, 0), (1, 0): (0, 1)}


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    size = len(lines[-1]), len(lines)
    obs = []
    for x in range(size[0]):
        for y in range(size[1]):
            if lines[y][x] == "#":
                obs.append((x, y))
            elif lines[y][x] == "^":
                start = (x, y)

    return size, start, obs


def parseB(file):
    return parseA(file)


def move(p, d):
    return (p[0] + d[0], p[1] + d[1])


def d6a(parsed, return_data=False):
    size, start, obs = parsed

    d = (0, -1)
    pos = start
    traveled = []
    while True:
        nex = move(pos, d)
        while nex in obs:
            d = right[d]
            nex = move(pos, d)

        traveled.append(pos)
        pos = nex

        if pos[0] * pos[1] < 0 or pos[0] >= size[0] or pos[1] >= size[1]:
            break

    if return_data:
        return traveled
    else:
        return len(set(traveled))


def chunks(xs, n):
    n = max(1, n)
    return (xs[i : i + n] for i in range(0, len(xs), n))


def d6b(parsed):
    def test_loop(chunk_jobs, result_dict):
        for coords in chunk_jobs:
            obs_extra = obs + [coords]
            d = (0, -1)
            pos = start
            traveled = {}
            while True:
                nex = move(pos, d)
                while nex in obs_extra:
                    d = right[d]
                    nex = move(pos, d)

                past = traveled.get(pos, None)
                if past is None:
                    traveled[pos] = [d]
                elif not d in past:
                    traveled[pos].append(d)
                else:
                    result_dict[coords] = True
                    break

                pos = nex
                if pos[0] * pos[1] < 0 or pos[0] >= size[0] or pos[1] >= size[1]:
                    result_dict[coords] = False
                    break

    manager = multiprocessing.Manager()
    return_dict = manager.dict()
    jobs = []

    size, start, obs = parsed
    travelled = d6a(parsed, True)
    n_proc = 12
    chunk_jobs = chunks(travelled, int(len(travelled) / n_proc))
    for c in chunk_jobs:
        p = multiprocessing.Process(target=test_loop, args=(c, return_dict))
        jobs.append(p)
        p.start()

    for p in jobs:
        p.join()

    return list(return_dict.values()).count(True)


testfile = "2024/inputs/day6testinput.txt"
file = "2024/inputs/day6input.txt"

print("TEST:")
start = time.time()
print(d6a(parseA(testfile)))
mid = time.time()
print(d6b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d6a(parseA(file)))
mid = time.time()
print(d6b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
