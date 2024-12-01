import time

def d20a(file):
    pass

def d20b(file):
    pass

file = "2024/inputs/day20input.txt"
start = time.time()
print(d20a(file))
mid = time.time()
print(d20b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")