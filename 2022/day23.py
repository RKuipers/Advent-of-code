import time
from collections import Counter

def vis(elves, r):
    elves.sort(key=lambda x: x.real)
    real_min = int(elves[0].real)
    real_range = int(elves[-1].real - elves[0].real)
    elves.sort(key=lambda x: x.imag)
    imag_min = int(elves[0].imag)
    imag_range = int(elves[-1].imag - elves[0].imag)
    
    print(f"Round {r+1}")
    for i in range(imag_min, imag_min + imag_range+1):
        print("".join(['#' if (j + i * 1j) in elves else '.' for j in range(real_min, real_min + real_range+1)]))
    print("")

checks = [[-1j, -1j+1, -1j-1], [1j, 1j+1, 1j-1], [-1, -1+1j, -1-1j], [1, 1+1j, 1-1j]]

def get_proposal(elf, elves, i):
    prop = None
    found_elf = False
    for j in range(4):
        pos = [c + elf for c in checks[(i+j)%4] if not c + elf in elves]
        if len(pos) < 3:
            found_elf = True
        if len(pos) == 3 and prop is None:
            prop = pos[0]
        if found_elf and prop is not None:
            return prop
        
    return elf

def d23p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    elves = {i * 1j + j for i, l in enumerate(lines) for j, c in enumerate(l.strip()) if c == '#'}
    
    for i in range(10):        
        proposals = {e: get_proposal(e, elves, i) for e in elves}
        
        c = Counter(proposals.values())
        
        new_pos = {e for e in proposals if c[proposals[e]]==1}
        blocked = elves - new_pos        
        
        elves = blocked | {proposals[e] for e in new_pos}

        # vis(elves, i)
    
    elves = list(elves)
    elves.sort(key=lambda x: x.real)
    real_range = elves[-1].real - elves[0].real + 1
    elves.sort(key=lambda x: x.imag)
    imag_range = elves[-1].imag - elves[0].imag + 1
    
    return real_range * imag_range - len(elves)
        
        
def d23p2(file) -> int:  
    with open(file) as f:
        lines = f.readlines()   
        
    elves = {i * 1j + j for i, l in enumerate(lines) for j, c in enumerate(l.strip()) if c == '#'}
    
    i = 0
    while True:        
        proposals = {e: get_proposal(e, elves, i) for e in elves}
        
        c = Counter(proposals.values())
        
        new_pos = {e for e in proposals if c[proposals[e]]==1}
        blocked = elves - new_pos        
        
        new_elves = blocked | {proposals[e] for e in new_pos}
        
        i = i + 1
        
        if elves == new_elves:
            return i
        
        elves = new_elves

        # vis(elves, i)
    
        
file = "day23input.txt"
start = time.time()
print(d23p1(file))
middle = time.time()
print(d23p2(file))
end = time.time()

print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")