import time

def d13a(file):
    pass

def d13b(file):
    pass

file = "2024/inputs/day13input.txt"
start = time.time()
print(d13a(file))
mid = time.time()
print(d13b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")