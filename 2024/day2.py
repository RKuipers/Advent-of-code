import time

def d2a(file):
    pass

def d2b(file):
    pass

file = "2024/inputs/day2input.txt"
start = time.time()
print(d2a(file))
mid = time.time()
print(d2b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")