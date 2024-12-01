import time

def d12a(file):
    pass

def d12b(file):
    pass

file = "2024/inputs/day12input.txt"
start = time.time()
print(d12a(file))
mid = time.time()
print(d12b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")