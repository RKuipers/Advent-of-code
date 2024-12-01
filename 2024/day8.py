import time

def d8a(file):
    pass

def d8b(file):
    pass

file = "2024/inputs/day8input.txt"
start = time.time()
print(d8a(file))
mid = time.time()
print(d8b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")