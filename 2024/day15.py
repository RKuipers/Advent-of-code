import time
from utils import Coord

directions = {">": Coord(1, 0), "<": Coord(-1, 0), "^": Coord(0, -1), "v": Coord(0, 1)}


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    boxes = []
    walls = []
    for y, l in enumerate(lines[1:]):
        if len(set(l.strip())) == 1:
            size = Coord(len(lines[0].strip())-1, y+1)
            break

        for x, c in enumerate(l.strip()[1:-1]):
            if c == "O":
                boxes.append(Coord(x+1, y+1))
            elif c == "#":
                walls.append(Coord(x+1, y+1))
            elif c == "@":
                start = Coord(x+1, y+1)

    move_chars = "".join([l.strip() for l in lines[y+2:]])
    moves = [directions[c] for c in move_chars]

    return start, size, boxes, walls, moves


def parseB(file):
    with open(file) as f:
        lines = f.readlines()

    boxes = {}
    walls = []
    for y, l in enumerate(lines[1:]):
        if len(set(l.strip())) == 1:
            size = Coord(2*(len(lines[0].strip())-1), y+1)
            break

        for x, c in enumerate(l.strip()[1:-1]):
            if c == "O":
                boxes[Coord(2*x+2, y+1)] = Coord(2*x+3, y+1)
                boxes[Coord(2*x+3, y+1)] = Coord(2*x+2, y+1)
            elif c == "#":
                walls.append(Coord(2*x+2, y+1))
                walls.append(Coord(2*x+3, y+1))
            elif c == "@":
                start = Coord(2*x+2, y+1)

    move_chars = "".join([l.strip() for l in lines[y+2:]])
    moves = [directions[c] for c in move_chars]

    return start, size, boxes, walls, moves

def d15a(parsed):
    start, size, boxes, walls, moves = parsed
    pos = start
    for m in moves:
        npos = pos + m
        cpos = npos
        while cpos in boxes:
            cpos += m
        if cpos in walls or not cpos > Coord(0, 0) or not cpos < size:
            continue
        pos = npos
        if npos in boxes:
            boxes.remove(npos)
            boxes.append(cpos)

    return sum([b.x + b.y *100 for b in boxes])

def visualise(pos, size, boxes, walls, m, i):
    print(i)
    for y in range(size.y+1):
        line = ["."] * (size.x+1)
        for x in range(size.x+1):
            c = Coord(x, y)
            if c in boxes and c <= boxes[c]:
                line[x] = '['
                line[x+1] = ']'
            elif c in walls or y == 0 or x <= 1 or y >= size.y or x >= size.x:
                line[x] = '#'
            elif c == pos:
                line[x] = '@'
        print("".join(line))
    print(next(key for key, value in directions.items() if value == m))

def d15b(parsed):
    start, size, boxes, walls, moves = parsed
    pos = start
    for i, m in enumerate(moves):
        #visualise(pos, size, boxes, walls, m, i)

        npos = pos + m
        invalid = npos in walls or not npos > Coord(1, 0) or not npos < size
        moved_boxes = set()
        new_box_locations = {}
        if not invalid and npos in boxes.keys():
            prev_boxes = {npos, boxes[npos]}
            moved_boxes = {npos, boxes[npos]}
            while len(prev_boxes) > 0:
                new_boxes = set()
                for b in prev_boxes:
                    bm = b+m
                    if bm in boxes and not bm in moved_boxes:
                        new_boxes.add(bm)
                        new_boxes.add(boxes[bm])
                    elif bm in walls or not bm > Coord(1, 0) or not bm < size:
                        invalid = True
                        break
                    if not bm in new_box_locations.keys():
                        new_box_locations[bm] = boxes[b]+m
                        new_box_locations[boxes[b]+m] = bm
                moved_boxes = moved_boxes.union(new_boxes)
                prev_boxes = new_boxes
        if invalid:
            continue

        pos = npos
        for b in moved_boxes:
            boxes.pop(b)
        for k, v in new_box_locations.items():
            boxes[k] = v

    return sum([b.x + b.y *100 for b in boxes if b <= boxes[b]])


testfile = "2024/inputs/day15testinput.txt"
file = "2024/inputs/day15input.txt"

print("TEST:")
start = time.time()
print(d15a(parseA(testfile)))
mid = time.time()
print(d15b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d15a(parseA(file)))
mid = time.time()
print(d15b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
