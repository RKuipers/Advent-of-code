import time

def d6a(file):
    pass

def d6b(file):
    pass

file = "2024/inputs/day6input.txt"
start = time.time()
print(d6a(file))
mid = time.time()
print(d6b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")