import time

def d17a(file):
    pass

def d17b(file):
    pass

file = "2024/inputs/day17input.txt"
start = time.time()
print(d17a(file))
mid = time.time()
print(d17b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")