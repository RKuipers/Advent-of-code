import time

def d18a(file):
    pass

def d18b(file):
    pass

file = "2024/inputs/day18input.txt"
start = time.time()
print(d18a(file))
mid = time.time()
print(d18b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")