import time

def d16a(file):
    pass

def d16b(file):
    pass

file = "2024/inputs/day16input.txt"
start = time.time()
print(d16a(file))
mid = time.time()
print(d16b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")