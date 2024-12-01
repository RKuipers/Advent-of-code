import time

def d10a(file):
    pass

def d10b(file):
    pass

file = "2024/inputs/day10input.txt"
start = time.time()
print(d10a(file))
mid = time.time()
print(d10b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")