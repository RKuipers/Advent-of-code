import time
from utils import Coord


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    a = int(lines[0].split(": ")[1])
    b = int(lines[1].split(": ")[1])
    c = int(lines[2].split(": ")[1])

    prog = list(map(int, lines[4].removeprefix("Program: ").split(",")))

    return prog, a, b, c


def parseB(file):
    return parseA(file)


def combo(x, a, b, c):
    return [0, 1, 2, 3, a, b, c][x]


def d17a(parsed, return_numbers=False):
    prog, a, b, c = parsed

    out = []

    pointer = 0
    while pointer < len(prog) - 1:
        op = prog[pointer]
        param = prog[pointer + 1]
        if op == 0:
            a = a // 2 ** combo(param, a, b, c)
        elif op == 1:
            b = b ^ param
        elif op == 2:
            b = combo(param, a, b, c) % 8
        elif op == 3 and a > 0:
            pointer = param
            continue
        elif op == 4:
            b = b ^ c
        elif op == 5:
            out.append(combo(param, a, b, c) % 8)
        elif op == 6:
            b = a // 2 ** combo(param, a, b, c)
        elif op == 7:
            c = a // 2 ** combo(param, a, b, c)
        pointer += 2

    if return_numbers:
        return out
    return ",".join(map(str, out))


"""
2,4,  set b to a % 8          ((((a % 8) XOR 7) XOR (a // 2^((a % 8) XOR 7))) XOR 4)%8
1,7,  set b to b XOR 7        (((b XOR 7) XOR (a // 2^(b XOR 7))) XOR 4)%8
7,5,  set c to a // 2^b       ((b XOR (a // 2^b)) XOR 4)%8
4,1,  set b to b XOR c        ((b XOR c) XOR 4)%8
1,4,  set b to b XOR 4        (b XOR 4)%8
5,5,  output b % 8            b % 8   
0,3,  set a to a // 8
3,0   goto start

((((a % 8) XOR 7) XOR (a // 2^((a % 8) XOR 7))) XOR 4)%8 == 2
((((a % 8) XOR 7) XOR (a // 2^((a % 8) XOR 7))) XOR 4) == 2
((a % 8) XOR 7) XOR (a // 2^((a % 8) XOR 7)) == 6
"""


def d17b(parsed):
    prog, a, b, c = parsed
    out = False
    start = 8**15
    first_eight = [int("0o13322340", base=8)]
    i = 0
    while i < 8**8:
        for j in first_eight:
            a = start + i * 8**8 + j
            out = d17a((prog, a, b, c), True)
            if len(out) == len(prog) and out == prog:
                print(f"{a}, {oct(a)}")
            i += 1

def d17b2(parsed):
    prog, a, b, c = parsed
    additions = {-1: {0}}
    for i in range(len(prog)):
        new_additions = set()
        for j in range(8):
            for addition in additions[i-1]:
                a = j*(8**i) + addition
                res = d17a((prog, a, b, c), True)
                if res[:max(0, i-2)] == prog[:max(0, i-2)]:
                    new_additions.add(a)
        additions[i] = set(new_additions)

    for x in sorted(list(additions[len(prog) - 1])):
        if prog == d17a((prog, x, b, c), True):
            return x


testfile = "2024/inputs/day17testinput.txt"
file = "2024/inputs/day17input.txt"

print("TEST:")
start = time.time()
print(d17a(parseA(testfile)))
mid = time.time()
print(d17b2(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d17a(parseA(file)))
mid = time.time()
print(d17b2(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
