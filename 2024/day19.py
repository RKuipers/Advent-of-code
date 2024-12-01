import time

def d19a(file):
    pass

def d19b(file):
    pass

file = "2024/inputs/day19input.txt"
start = time.time()
print(d19a(file))
mid = time.time()
print(d19b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")