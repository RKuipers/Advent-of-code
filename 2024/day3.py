import time

def d3a(file):
    pass

def d3b(file):
    pass

file = "2024/inputs/day3input.txt"
start = time.time()
print(d3a(file))
mid = time.time()
print(d3b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")