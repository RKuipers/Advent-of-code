import copy
import sys
from collections import deque, defaultdict
from functools import cache
import random

ans = 0
st = False

all_ops = set()
values = {}
gates = {}

random.seed(0)

file = "2024/inputs/day24input.txt"
with open(file) as f:
    lines = f.readlines()

for line in lines:
    if line.strip() == "":
        st = True
        continue
    if st:
        op1, op, op2, arrow, out_op = line.strip().split()
        gates[out_op] = (op1, op, op2)
        all_ops.add(out_op)
    else:
        op, val = line.strip().split()
        values[op[:-1]] = int(val)
        values[op[:-1]] = random.random() < 0.5
        all_ops.add(op[:-1])

@cache
def evaluate(op):
    if op in values:
        return values[op]
    op1 = evaluate(gates[op][0])
    op2 = evaluate(gates[op][2])
    if gates[op][1] == "AND":
        return op1 & op2
    if gates[op][1] == "OR":
        return op1 | op2
    if gates[op][1] == "XOR":
        return op1 ^ op2


@cache
def inspect(op, depth=0):
    if op in values or depth >= 3:
        return op

    op1 = inspect(gates[op][0], depth + 1)
    op2 = inspect(gates[op][2], depth + 1)
    if gates[op][1] == "AND":
        return f"{op}{{{op1} & {op2}}}"
    if gates[op][1] == "OR":
        return f"{op}{{{op1} | {op2}}}"
    if gates[op][1] == "XOR":
        return f"{op}{{{op1} ^ {op2}}}"


def get_num(beg):
    digs = {}
    for op in all_ops:
        if op.startswith(beg):
            digs[int(op[1:])] = evaluate(op)

    x = max([k for k in digs])
    num = 0
    for i in range(x, -1, -1):
        num = 2 * num + digs[i]
    return num


swaps = [("wrm", "wss"), ("gbs", "z29"), ("thm", "z08"), ('z22', 'hwq')]
for a, b in swaps:
    tb = gates[a]
    ta = gates[b]
    gates[a] = ta
    gates[b] = tb

# Code I used to figure out which bits were wrong:
x = get_num("x")
y = get_num("y")
z = get_num("z")
print(bin(x + y))
print(bin(z))
for i, (b1, b2) in enumerate(zip(bin(x + y), bin(z))):
    if b1 != b2:
        print(f"Bit {47-i} is wrong")
# assert x+y ==z

# Code I used to figure out how bit 14 was wrong:
# print(inspect("z12"))
# print(inspect("z13"))
# print(inspect("z14"))
# print(inspect("z15"))
# wss <-> wrm

# # Code I used to figure out how bit 29 was wrong:
# print(inspect("z25"))
# print(inspect("z26"))
# print(inspect("z27"))
# print(inspect("z28"))
# print(inspect("z29"))
# print(inspect("z30"))
# print(inspect("z31"))
# print(
#     evaluate("z29"), evaluate("y29"), evaluate("x29"), evaluate("pgq"), evaluate("dgr")
# )
# z29 <-> gbs

# # Code I used to figure out how bit 9 was wrong:
# print(inspect("z06"))
# print(inspect("z07"))
# print(inspect("z08"))
# print(inspect("z09"))
# print(inspect("z10"))
# thm <-> z08

# # Code I used to figure out how bit 29 was wrong:
# print(inspect("z21"))
# print(inspect("z22"))
# print(inspect("z23"))
# z22 <-> hqw
