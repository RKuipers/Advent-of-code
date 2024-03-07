import time

def d1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    return len([i for i, l in enumerate(lines[1:]) if int(lines[i-1].strip()) < int(lines[i].strip())]) + 1
    
        
file = "d1.txt"
start = time.time()
print(d1(file))
middle = time.time()
print(d1(file))
end = time.time()

print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")