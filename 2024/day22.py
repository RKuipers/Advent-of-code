import time

def d22a(file):
    pass

def d22b(file):
    pass

file = "2024/inputs/day22input.txt"
start = time.time()
print(d22a(file))
mid = time.time()
print(d22b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")