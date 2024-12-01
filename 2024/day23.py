import time

def d23a(file):
    pass

def d23b(file):
    pass

file = "2024/inputs/day23input.txt"
start = time.time()
print(d23a(file))
mid = time.time()
print(d23b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")