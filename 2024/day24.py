import copy
import time
from utils import Coord


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    inits = {}

    for i, l in enumerate(lines):
        if ":" in l:
            splt = l.strip().split(": ")
            inits[splt[0]] = splt[1] == "1"
        else:
            break

    gates = {}

    for l in lines[i + 1 :]:
        splt = l.strip().split(" ")
        gates[splt[-1]] = (splt[0], splt[2], splt[1])

    return inits, gates


def parseB(file):
    return parseA(file)


def lookup(g, gates, truth):
    if g in truth.keys():
        return truth

    gate = gates[g]
    if gate[2] == "AND":
        truth = lookup(gate[0], gates, truth)
        truth = lookup(gate[1], gates, truth)
        truth[g] = truth[gate[0]] and truth[gate[1]]
    elif gate[2] == "OR":
        truth = lookup(gate[0], gates, truth)
        if not truth[gate[0]]:
            lookup(gate[1], gates, truth)
        truth[g] = truth[gate[0]] or truth[gate[1]]
    elif gate[2] == "XOR":
        truth = lookup(gate[0], gates, truth)
        truth = lookup(gate[1], gates, truth)
        truth[g] = truth[gate[0]] != truth[gate[1]]

    return truth


def d24a(parsed):
    inits, gates = parsed

    zs = []
    truth = copy.copy(inits)

    for g in gates:
        if g[0] == "z":
            zs.append(g)
            truth = lookup(g, gates, truth)

    return int("".join(["1" if truth[z] else "0" for z in sorted(zs, reverse=True)]), 2)


def construct_xy_gate(g, gates):
    gate = gates[g]
    new_gates = copy.copy(gates)
    if isinstance(gate[0], str) and not gate[0][0] in ["x", "y"]:
        new_gates[gate[0]] = construct_xy_gate(gate[0], gates)
    if isinstance(gate[1], str) and not gate[1][0] in ["x", "y"]:
        new_gates[gate[1]] = construct_xy_gate(gate[1], gates)

    new_gates[g] = (
        new_gates.get(gate[0], gate[0]),
        new_gates.get(gate[1], gate[1]),
        gate[2],
    )
    return new_gates[g]


def check_equals(g, gates, val):
    gate = gates[g]
    if isinstance(val, tuple) and val[2] == "AND":
        # if XOR, recursive XOR
        # if OR, recursive AND
        # if AND, recursive x, recursive y
        if isinstance(gate[0], tuple):
            check_equals(gate[0], gates, None)


# z00 <-> rhk


def d24b(parsed):
    inits, gates = parsed

    zs = []
    truth = copy.copy(inits)

    for g in gates:
        if g[0] == "z":
            zs.append(g)
            truth = lookup(g, gates, truth)

    return "".join(["1" if truth[z] else "0" for z in sorted(zs, reverse=True)])


testfile = "2024/inputs/day24testinput.txt"
file = "2024/inputs/day24input.txt"

print("TEST:")
start = time.time()
print(d24a(parseA(testfile)))
mid = time.time()
# print(d24b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d24a(parseA(file)))
mid = time.time()
print(d24b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
