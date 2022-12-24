import time
import math

def lcm(a, b):
    return abs(a*b) // math.gcd(a, b)

def vis(blizz, pos, width, heigth):
    for y in range(heigth):
        s = []
        for x in range(width):
            p = x + y * 1j
            if p == pos:
                s.append("E")
            elif p in blizz:
                s.append("b")
            else:
                s.append(".")
        print("".join(s))
        
    print("")

dirs = [1j, 1, 0, -1, -1j]
#dirs = [-1j, -1, 0, 1, 1j]

def get_blizz(blizzards, c, width, height):
    blizz = {b + d * c for b, d in blizzards.items()}
    return {b.real % width + (b.imag % height) * 1j for b in blizz} 

def get_dis_to_end(pos, end):
    return (end - pos).real + (end - pos).imag

def find_path(start, end, blizzards, cycle, cycle_start, width, height, dirs):
    to_explore_all = {cycle_start: (start, [])}
    explored = {c: [] for c in range(cycle)}
    
    c = cycle_start
    to_explore = [(start, [])]
    
    while True:
        to_explore_next = []
        while len(to_explore) > 0:
            (pos, path) = to_explore[0]
            to_explore = to_explore[1:]
            
            c_next = (c+1)%cycle
            
            pos_next = {pos + d: d for d in dirs if (pos + d) == end or (pos + d) == start or ((pos + d).real >=0 and (pos + d).imag >= 0 and (pos + d).real < width and (pos + d).imag < height)}
            
            if end in pos_next.keys():
                print(f"Found path of length {len(path) + 1}")
                return len(path) + 1
                 
            pos_next_filtered = list(filter(lambda p : p not in blizzards[c_next], pos_next.keys()))
            
            to_explore_next.extend([(p, path + [d]) for p, d in pos_next.items() if p in pos_next_filtered and p not in explored[c_next]])
        to_explore = to_explore_next
        c = (c+1)%cycle


def d24p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    blizzards_initial = dict()
    height = len(lines) - 2
    width = len(lines[0].strip()) - 2
    cycle = lcm(height, width)
        
    for y, l in enumerate(lines):
        for x, c in enumerate(l.strip()):
            if c == '>':
                blizzards_initial.update({x-1 + (y-1) * 1j: 1})
            elif c == '<':
                blizzards_initial.update({x-1 + (y-1) * 1j: -1})
            elif c == '^':
                blizzards_initial.update({x-1 + (y-1) * 1j: -1j})
            elif c == 'v':
                blizzards_initial.update({x-1 + (y-1) * 1j: 1j})
    
    blizzards = []
    for c in range(cycle):
        blizzards.append(get_blizz(blizzards_initial, c, width, height))
    
    start = -1j
    end = height * 1j + width - 1
            
    return find_path(start, end, blizzards, cycle, 0, width, height, [1j, 1, 0, -1, -1j])
        
def d24p2(file) -> int:  
    with open(file) as f:
        lines = f.readlines() 
        
    blizzards_initial = dict()
    height = len(lines) - 2
    width = len(lines[0].strip()) - 2
    cycle = lcm(height, width)
        
    for y, l in enumerate(lines):
        for x, c in enumerate(l.strip()):
            if c == '>':
                blizzards_initial.update({x-1 + (y-1) * 1j: 1})
            elif c == '<':
                blizzards_initial.update({x-1 + (y-1) * 1j: -1})
            elif c == '^':
                blizzards_initial.update({x-1 + (y-1) * 1j: -1j})
            elif c == 'v':
                blizzards_initial.update({x-1 + (y-1) * 1j: 1j})
    
    blizzards = []
    for c in range(cycle):
        blizzards.append(get_blizz(blizzards_initial, c, width, height))
    
    start = -1j
    end = height * 1j + width - 1
    
    dirs_there = [1j, 1, 0, -1, -1j]
    dirs_back = [-1j, -1, 0, 1, 1j]
                
    p1 = find_path(start, end, blizzards, cycle, 0, width, height, dirs_there)
    p2 = find_path(end, start, blizzards, cycle, p1%cycle, width, height, dirs_back)
    p3 = find_path(start, end, blizzards, cycle, (p1+p2)%cycle, width, height, dirs_there)
    
    return p1 + p2 + p3
    
        
file = "day24input.txt"
start = time.time()
print(f"Part 1: {d24p1(file)}")
middle = time.time()
print(f"Part 2: {d24p2(file)}")
end = time.time()

print("")
print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")