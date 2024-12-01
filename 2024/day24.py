import time

def d24a(file):
    pass

def d24b(file):
    pass

file = "2024/inputs/day24input.txt"
start = time.time()
print(d24a(file))
mid = time.time()
print(d24b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")