import time

def d9a(file):
    pass

def d9b(file):
    pass

file = "2024/inputs/day9input.txt"
start = time.time()
print(d9a(file))
mid = time.time()
print(d9b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")