import time

def d14a(file):
    pass

def d14b(file):
    pass

file = "2024/inputs/day14input.txt"
start = time.time()
print(d14a(file))
mid = time.time()
print(d14b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")