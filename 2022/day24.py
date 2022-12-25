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
    return abs(int((end - pos).real) + int((end - pos).imag))

def get_increased_distance(dirs, d):
    if d == dirs[0] or d == dirs[1]:
        return 0
    if d == dirs[2]:
        return 1
    if d == dirs[3] or d == dirs[4]:
        return 2

def find_path_2(start, end, blizzards, cycle, cycle_start, width, height, dirs):
    dis = get_dis_to_end(start, end)
    to_explore = {dis: [(start, cycle_start)]}

    while True:
        while len(to_explore[dis]) > 0:
            pos, c = to_explore[dis].pop()
            
            for d in dirs:
                pos_next = pos + d
                if pos_next == end:
                    print(f"Returning {dis}")
                    return dis
                
                if pos_next.real < 0 or pos_next.real >= width or pos_next.imag < 0 or pos_next.imag >= height:
                    if pos_next != start:
                        continue
                c_next = (c+1)%cycle
                if pos_next in blizzards[c_next]:
                    continue
                entry = (pos_next, c_next)
                dis_next = dis + get_increased_distance(dirs, d)
                if dis_next not in to_explore.keys():
                    to_explore.update({dis_next: []})
                if entry in to_explore[dis_next]:
                    continue
                to_explore[dis_next].append(entry)
        dis = dis + 1
                

def find_path(start, end, blizzards, cycle, cycle_start, width, height, dirs):
    dis = get_dis_to_end(start, end)
    to_explore_all = {dis + d: [] for d in range(dis*2)}
    to_explore_all[dis].append((start, cycle_start))    
    
    while True:
        while len(to_explore_all[dis]) > 0:
            pos, c = to_explore_all[dis][0]
            to_explore_all[dis] = to_explore_all[dis][1:]
            
            c_next = (c+1)%cycle
            
            pos_next = {pos + d: d for d in dirs if (pos + d) == end or (pos + d) == start or ((pos + d).real >=0 and (pos + d).imag >= 0 and (pos + d).real < width and (pos + d).imag < height)}
            
            if end in pos_next.keys():
                print(f"Found path of length {dis}")
                return dis
                 
            pos_next_filtered = list(filter(lambda p : p not in blizzards[c_next], pos_next.keys()))
            to_queue = {(p, c_next, dis + get_increased_distance(dirs, d)) for p, d in pos_next.items() if p in pos_next_filtered}
            
            for p_, c_, d_ in to_queue:
                if (p_, c_) not in to_explore_all[d_]:
                    to_explore_all[d_].append((p_, c_))
                
        dis = dis + 1

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
        
def d24p2(file, pathfinder) -> int:  
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
middle_1 = time.time()
print(f"Part 2a: {d24p2(file, find_path)}")
middle_2 = time.time()
print(f"Part 2b: {d24p2(file, find_path_2)}")
end = time.time()

print("")
print(f"Part 1 runs in {middle_1 - start}s")
print(f"Part 2a runs in {middle_2 - middle_1}s")
print(f"Part 2b runs in {end - middle_2}s")