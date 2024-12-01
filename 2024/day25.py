import time

def d25a(file):
    pass

def d25b(file):
    pass

file = "2024/inputs/day25input.txt"
start = time.time()
print(d25a(file))
mid = time.time()
print(d25b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")