import time
from utils import Coord


def parseA(file):
    with open(file) as f:
        lines = f.readlines()

    robots = []
    for l in lines:
        spl = l.removeprefix("p=").split(" v=")
        [px, py] = spl[0].split(",")
        [vx, vy] = spl[1].split(",")
        robots.append((Coord(int(px), int(py)), Coord(int(vx), int(vy))))
    return robots


def parseB(file):
    return parseA(file)


def d14a(parsed, size):
    x_mid = size.x // 2
    y_mid = size.y // 2
    q1 = 0
    q2 = 0
    q3 = 0
    q4 = 0
    for p, v in parsed:
        distance = v * 100 + p
        end_pos = Coord(distance.x % size.x, distance.y % size.y)
        if end_pos.x < x_mid and end_pos.y < y_mid:
            q1 += 1
        elif end_pos.x > x_mid and end_pos.y < y_mid:
            q2 += 1
        elif end_pos.x < x_mid and end_pos.y > y_mid:
            q3 += 1
        elif end_pos.x > x_mid and end_pos.y > y_mid:
            q4 += 1
    return q1 * q2 * q3 * q4


def d14b(parsed, size):
    pos = set()
    i = 0
    while len(pos) < len(parsed):
        i += 1
        pos = set()
        for p, v in parsed:
            distance = v * i + p
            end_pos = Coord(distance.x % size.x, distance.y % size.y)
            pos.add(end_pos)
    return i


testfile = "2024/inputs/day14testinput.txt"
file = "2024/inputs/day14input.txt"

print("TEST:")
start = time.time()
print(d14a(parseA(testfile), Coord(11, 7)))
mid = time.time()
print(d14b(parseB(testfile), Coord(11, 7)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d14a(parseA(file), Coord(101, 103)))
mid = time.time()
print(d14b(parseB(file), Coord(101, 103)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
