import time

def d7a(file):
    pass

def d7b(file):
    pass

file = "2024/inputs/day7input.txt"
start = time.time()
print(d7a(file))
mid = time.time()
print(d7b(file))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")