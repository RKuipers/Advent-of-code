import time

def d21a(file):
    pass

def d21b(file):
    pass

file = "2024/inputs/day21input.txt"
start = time.time()
print(d21a(file))
mid = time.time()
print(d21b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")