import time

def d4a(file):
    pass

def d4b(file):
    pass

file = "2024/inputs/day4input.txt"
start = time.time()
print(d4a(file))
mid = time.time()
print(d4b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")