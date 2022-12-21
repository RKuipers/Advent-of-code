directions = [(0, 0, 1), (0, 0, -1), (0, 1, 0), (0, -1, 0), (1, 0, 0), (-1, 0, 0)]

def get_neighbors(block):
    return [tuple(x + y for x, y in zip(block, d)) for d in directions]    

def d18p1(file) -> int:    
    with open(file) as f:
        lines = f.readlines()
        
    blocks = set()
    area = 0    
        
    for l in lines:
        splt = l.strip().split(",")
        blocks.add((int(splt[0]), int(splt[1]), int(splt[2])))
        
    for block in blocks:
        neighbors = get_neighbors(block)
        area = area + 6 - len(blocks.intersection(neighbors))
        
    return area

def is_outside(blocks, block, min_coords, max_coords):    
    stack = [n for n in get_neighbors(block) if n not in blocks]
    explored = set()
    found_edge = False
    while not found_edge and len(stack) > 0:
        n = stack.pop()
        if n[0] < min_coords[0] or n[0] > max_coords[0] or n[1] < min_coords[1] or n[1] > max_coords[1] or n[2] < min_coords[2] or n[2] > max_coords[2]:
            found_edge = True
            break
        explored.add(n)
        neighs = get_neighbors(n)
        for neighbor in neighs:
            if neighbor not in blocks and neighbor not in stack and neighbor not in explored:
                stack.append(neighbor)
        
    return found_edge

def d18p2(file) -> int:    
    with open(file) as f:
        lines = f.readlines()
        
    blocks = set()
    area = 0

    min_coords = [1000000, 1000000, 1000000]
    max_coords = [-1000000, -1000000, -1000000]
        
    for l in lines:
        splt = l.strip().split(",")
        block = (int(splt[0]), int(splt[1]), int(splt[2]))
        min_coords[0] = min(min_coords[0], block[0])
        min_coords[1] = min(min_coords[1], block[1])
        min_coords[2] = min(min_coords[2], block[2])
        max_coords[0] = max(max_coords[0], block[0])
        max_coords[1] = max(max_coords[1], block[1])
        max_coords[2] = max(max_coords[2], block[2])
        blocks.add(block)
        
    print(f"Blocks: {len(blocks)}")
        
    i = 0
    for block in blocks:
        if i % 100 == 0:
            print(f"On block: {i}")
        neighbors = get_neighbors(block)  
        area = area + len({n for n in neighbors if n not in blocks and is_outside(blocks, n, min_coords, max_coords)})
        i = i + 1
        
    return area
   
file = "day18input.txt"
print(d18p1(file))
print(d18p2(file))