from functools import cache
import time
from utils import Coord

@cache
def next_code(x):
    x = (x ^ (x * 64)) % 16777216
    x = (x ^ (x // 32)) % 16777216
    x = (x ^ (x * 2048)) % 16777216
    return x

def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    return list(map(int, lines))


def parseB(file):
    return parseA(file)


def d22a(init_numbers):
    final_numbers = []
    for n in init_numbers:
        x = n
        for _ in range(2000):
            x = next_code(x)
        final_numbers.append(x)
    return sum(final_numbers)


def d22b(init_numbers):
    changes = {}
    values = {}
    all_codes = set()
    code_values = {}
    for n in init_numbers:
        changes[n] = []
        values[n] = {}
        x = n
        for _ in range(2000):
            x_new = next_code(x)
            changes[n].append((x_new % 10) - (x % 10)) 
            x = x_new   

            code = tuple(changes[n][-4:])

            if len(code) == 4 and not code in values[n].keys():
                values[n][code] = x_new % 10
                all_codes.add(code)

    for code in all_codes:
        code_values[code] = sum(v.get(code, 0) for v in values.values())

    max_code = max(code_values, key=code_values.get)

    return max_code, code_values[max_code]
                


testfile = "2024/inputs/day22testinput.txt"
file = "2024/inputs/day22input.txt"

print("TEST:")
start = time.time()
print(d22a(parseA(testfile)))
mid = time.time()
print(d22b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d22a(parseA(file)))
mid = time.time()
print(d22b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
