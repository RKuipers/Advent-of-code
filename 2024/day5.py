import time

def d5a(file):
    pass

def d5b(file):
    pass

file = "2024/inputs/day5input.txt"
start = time.time()
print(d5a(file))
mid = time.time()
print(d5b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")