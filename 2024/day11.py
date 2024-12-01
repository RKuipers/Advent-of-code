import time

def d11a(file):
    pass

def d11b(file):
    pass

file = "2024/inputs/day11input.txt"
start = time.time()
print(d11a(file))
mid = time.time()
print(d11b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")