import time

def d15a(file):
    pass

def d15b(file):
    pass

file = "2024/inputs/day15input.txt"
start = time.time()
print(d15a(file))
mid = time.time()
print(d15b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")