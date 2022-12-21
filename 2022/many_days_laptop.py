import time


def d1p1(banned_numbers=[]) -> int:
    with open("day_1_input.txt") as f:
        lines = f.readlines()

    max_elf = 0
    this_elf = 0
    for l in lines:
        if l == "\n":
            if not this_elf in banned_numbers:
                max_elf = max(max_elf, this_elf)
            this_elf = 0
        else:
            this_elf = this_elf + int(l[:-1])

    return max_elf


def d1p2() -> int:
    first = d1p1([])
    second = d1p1([first])
    third = d1p1([first, second])

    return first + second + third


def rps_win(opp: int, me: int):
    if opp == me:
        return (3, 3)
    elif opp > me:
        if opp == 3 and me == 1:
            return (0, 6)
        else:
            return (6, 0)
    else:
        if opp == 1 and me == 3:
            return (6, 0)
        else:
            return (0, 6)


def d2p1() -> int:
    mapping = {"A": 1, "B": 2, "C": 3, "X": 1, "Y": 2, "Z": 3}

    with open("day2input.txt") as f:
        lines = f.readlines()

    me_total = 0

    for l in lines:
        split = l.split(" ")
        opp = mapping[split[0]]
        me = mapping[split[1][0]]

        win = rps_win(opp, me)

        opp = opp + win[0]
        me = me + win[1]

        me_total = me_total + me

    return me_total


def d2p2() -> int:
    mapping = {"A": 1, "B": 2, "C": 3, "X": (6, 0), "Y": (3, 3), "Z": (0, 6)}

    with open("day2input.txt") as f:
        lines = f.readlines()

    me_total = 0

    for l in lines:
        split = l.split(" ")
        opp = mapping[split[0]]
        outcome = mapping[split[1][0]]

        options = {me: rps_win(opp, me) == outcome for me in [1, 2, 3]}
        me = outcome[1] + [k for k, v in options.items() if v][0]

        me_total = me_total + me

    return me_total


def d3p1() -> int:
    with open("day3input.txt") as f:
        lines = f.readlines()

    segments = [
        (line[: int(len(line) / 2)], line[int(len(line) / 2) : -1]) for line in lines
    ]
    dupes = [[c for c in a if c in b][0] for a, b in segments]
    values = [ord(l) - 38 if l.isupper() else ord(l) - 96 for l in dupes]
    return sum(values)


def d3p2() -> int:
    with open("day3input.txt") as f:
        lines = f.readlines()

    dupes = []
    for i in range(int(len(lines) / 3)):
        l1 = lines[i * 3][:-1]
        l2 = lines[i * 3 + 1][:-1]
        l3 = lines[i * 3 + 2][:-1]

        dupes.append([c for c in l1 if c in l2 and c in l3][0])

    values = [ord(l) - 38 if l.isupper() else ord(l) - 96 for l in dupes]
    return sum(values)


def check_contained(start1, start2, end1, end2) -> bool:
    if start1 < start2:
        return end2 <= end1
    elif start1 > start2:
        return end2 >= end1
    else:
        return True


def check_overlap(start1, start2, end1, end2) -> bool:
    return start1 <= end2 and end1 >= start2


def d4p1() -> int:
    with open("day4input.txt") as f:
        lines = f.readlines()

    count = 0

    for l in lines:
        splt = l.split(",")
        s1 = int(splt[0].split("-")[0])
        e1 = int(splt[0].split("-")[1])
        s2 = int(splt[1].split("-")[0])
        e2 = int(splt[1].split("-")[1][:-1])

        if check_contained(s1, s2, e1, e2):
            count = count + 1

    return count


def d4p2() -> int:
    with open("day4input.txt") as f:
        lines = f.readlines()

    count = 0

    for l in lines:
        splt = l.split(",")
        s1 = int(splt[0].split("-")[0])
        e1 = int(splt[0].split("-")[1])
        s2 = int(splt[1].split("-")[0])
        e2 = int(splt[1].split("-")[1][:-1])

        if check_overlap(s1, s2, e1, e2):
            count = count + 1

    return count


def d5p1() -> str:
    with open("day5input.txt") as f:
        lines = f.readlines()

    stacks = {x + 1: [] for x in range(9)}

    for line in lines:
        if line[1] == "1":
            break
        boxes = {i: c for i, c in enumerate(line) if c.isupper()}
        for i, c in boxes.items():
            stack = int((i - 1) / 4) + 1
            stacks[stack].insert(0, c)

    for line in lines:
        if not "move" in line:
            continue
        splt = line.split(" ")
        for i in range(int(splt[1])):
            box = stacks[int(splt[3])].pop()
            stacks[int(splt[5][:-1])].append(box)

    return "".join([stack[-1] for stack in stacks.values()])


def d5p2() -> str:
    with open("day5input.txt") as f:
        lines = f.readlines()

    stacks = {x + 1: [] for x in range(9)}

    for line in lines:
        if line[1] == "1":
            break
        boxes = {i: c for i, c in enumerate(line) if c.isupper()}
        for i, c in boxes.items():
            stack = int((i - 1) / 4) + 1
            stacks[stack].insert(0, c)

    for line in lines:
        if not "move" in line:
            continue
        splt = line.split(" ")
        n = int(splt[1])
        ori = int(splt[3])
        boxes = stacks[ori][-n:]
        stacks[ori] = stacks[ori][:-n]
        stacks[int(splt[5][:-1])].extend(boxes)

    return "".join([stack[-1] for stack in stacks.values()])


def d6p1() -> int:
    with open("day6input.txt") as f:
        lines = f.readlines()

    for i in range(4, len(lines[0])):
        if len(set(lines[0][i - 4 : i])) == 4:
            return i


def d6p2() -> int:
    with open("day6input.txt") as f:
        lines = f.readlines()

    for i in range(14, len(lines[0])):
        if len(set(lines[0][i - 14 : i])) == 14:
            return i


dir_sizes = {}


def get_dir_size(dirs: dict, dir: str):
    size = sum([int(item.split(" ")[0]) for item in dirs[dir] if item[:3] != "dir"])

    for item in dirs[dir]:
        splt = item.split(" ")
        if splt[0] == "dir":
            dir_name = f"{dir}-{splt[1]}"
            if dir_name in dir_sizes.keys():
                size = size + dir_sizes[dir_name]
            else:
                size = size + get_dir_size(dirs, dir_name)

    dir_sizes.update({dir: size})
    return size


def d7p1() -> int:
    with open("day7input.txt") as f:
        lines = f.readlines()

    dirs = {}
    curr = None
    i = 0
    while i < len(lines):
        if lines[i][:4] == "$ cd":
            dir = lines[i][5:-1]
            if dir == "..":
                path = curr.split("-")
                curr = "-".join(path[:-1])
            else:
                if curr is None:
                    curr = dir
                else:
                    curr = f"{curr}-{dir}"
            i = i + 1
        elif lines[i] == "$ ls\n":
            i = i + 1
            in_dir = []
            while lines[i][0] != "$":
                in_dir.append(lines[i][:-1])
                i = i + 1
                if i >= len(lines):
                    break
            dirs.update({curr: in_dir})
        else:
            print("ERROR 0")

    get_dir_size(dirs, "/")
    return sum([size for size in dir_sizes.values() if size <= 100000])


def d7p2() -> int:
    if dir_sizes == {}:
        d7p1()

    space_needed = 30000000 - (70000000 - dir_sizes["/"])

    return min([size for size in dir_sizes.values() if size >= space_needed])


def find_char(c: str, lines: list):
    c_dict = {i: line.find(c) for i, line in enumerate(lines)}
    c_line = max(c_dict, key=c_dict.get)
    return (c_line, c_dict[c_line])


def get_neighbors(field: list, pos, width: int, height: int, reverse=False) -> list:
    neighbor_coords = [
        (pos[0] - 1, pos[1]),
        (pos[0] + 1, pos[1]),
        (pos[0], pos[1] - 1),
        (pos[0], pos[1] + 1),
    ]
    filtered_coords = filter(
        lambda p: p[0] >= 0 and p[0] < height and p[1] >= 0 and p[1] < width,
        neighbor_coords,
    )
    if not reverse:
        return filter(
            lambda p: field[p[0]][p[1]] <= field[pos[0]][pos[1]] + 1,
            filtered_coords,
        )
    else:
        return filter(
            lambda p: field[p[0]][p[1]] >= field[pos[0]][pos[1]] - 1,
            filtered_coords,
        )


def d12p1() -> int:
    with open("day12input.txt") as f:
        lines = f.readlines()

    width = len(lines[0][:-1])
    heigth = len(lines)

    S = find_char("S", lines)
    E = find_char("E", lines)

    to_height = lambda x: 1 if x == "S" else (26 if x == "E" else ord(x) - 96)
    field = [[to_height(x) for x in line[:-1]] for line in lines]

    dis_field = [[-1 for _ in range(width)] for _ in range(heigth)]
    dis_field[S[0]][S[1]] = 0
    at_dis = [[S]]

    while dis_field[E[0]][E[1]] == -1:
        max_dis = at_dis[-1]
        at_dis.append([])
        for pos in max_dis:
            for neigh in get_neighbors(field, pos, width, heigth):
                if dis_field[neigh[0]][neigh[1]] == -1:
                    dis_field[neigh[0]][neigh[1]] = dis_field[pos[0]][pos[1]] + 1
                    at_dis[-1].append(neigh)

    path = ["E"]
    pos = E
    while "S" not in path:
        neighs = list(get_neighbors(field, pos, width, heigth, True))
        pos = list(
            filter(
                lambda p: dis_field[p[0]][p[1]] == dis_field[pos[0]][pos[1]] - 1, neighs
            )
        )[0]
        path.append(lines[pos[0]][pos[1]])

    return dis_field[E[0]][E[1]]


def d12p2() -> int:
    with open("day12input.txt") as f:
        lines = f.readlines()

    width = len(lines[0][:-1])
    heigth = len(lines)

    S = find_char("E", lines)

    to_height = lambda x: 1 if x == "S" else (26 if x == "E" else ord(x) - 96)
    field = [[to_height(x) for x in line[:-1]] for line in lines]

    dis_field = [[-1 for _ in range(width)] for _ in range(heigth)]
    dis_field[S[0]][S[1]] = 0
    at_dis = [[S]]

    first_a = (-1, -1)

    while first_a == (-1, -1):
        max_dis = at_dis[-1]
        at_dis.append([])
        for pos in max_dis:
            for neigh in get_neighbors(field, pos, width, heigth, True):
                if dis_field[neigh[0]][neigh[1]] == -1:
                    dis_field[neigh[0]][neigh[1]] = dis_field[pos[0]][pos[1]] + 1
                    at_dis[-1].append(neigh)

                    if field[neigh[0]][neigh[1]] == 1:
                        first_a = (neigh[0], neigh[1])

    return dis_field[first_a[0]][first_a[1]]


from parse import parse
from interval import interval


def get_man_dis(a, b):
    return abs((a - b).real) + abs((a - b).imag)


def get_row_interval(inputs, row):
    covered = interval()

    for inp in inputs:
        dis_row = int(abs(row - inp[0].imag))
        dis_bea = int(get_man_dis(inp[0], inp[1]))
        if dis_row > dis_bea:
            continue

        x = int(inp[0].real)
        covered = covered | interval[x - (dis_bea - dis_row), x + (dis_bea - dis_row)]

    return covered


def list_man_dis_points(pos, dis, low_bound, up_bound):
    return list(
        filter(
            lambda p: p.real >= low_bound
            and p.real <= up_bound
            and p.imag >= low_bound
            and p.imag <= up_bound,
            {pos + i + (dis - i) * 1j for i in range(dis + 1)}
            | {pos - i + (dis - i) * 1j for i in range(dis + 1)}
            | {pos + i - (dis - i) * 1j for i in range(dis + 1)}
            | {pos - i - (dis - i) * 1j for i in range(dis + 1)},
        )
    )


def d15p1() -> int:
    with open("day15input.txt") as f:
        lines = f.readlines()

    inputs = []

    for line in lines:
        vars = parse(
            "Sensor at x={sx}, y={sy}: closest beacon is at x={bx}, y={by}",
            line.strip(),
        ).named
        inputs.append(
            (
                int(vars["sx"]) + int(vars["sy"]) * 1j,
                int(vars["bx"]) + int(vars["by"]) * 1j,
            )
        )

    row = 2000000
    covered = get_row_interval(inputs, row)

    return sum([segment[1] - segment[0] + 1 for segment in covered]) - len(
        {inp[1] for inp in inputs if inp[1].imag == row}
    )


def d15p2() -> int:
    with open("day15input.txt") as f:
        lines = f.readlines()

    inputs = []

    for line in lines:
        vars = parse(
            "Sensor at x={sx}, y={sy}: closest beacon is at x={bx}, y={by}",
            line.strip(),
        ).named
        inputs.append(
            (
                int(vars["sx"]) + int(vars["sy"]) * 1j,
                int(vars["bx"]) + int(vars["by"]) * 1j,
            )
        )

    for y in range(4000000):
        covered = get_row_interval(inputs, y)
        if len(covered) == 2 and covered[0][1] + 2 == covered[1][0]:
            return y + (covered[0][1] + 1) * 4000000


def d15p2b() -> int:
    with open("day15input.txt") as f:
        lines = f.readlines()

    inputs = []

    for line in lines:
        vars = parse(
            "Sensor at x={sx}, y={sy}: closest beacon is at x={bx}, y={by}",
            line.strip(),
        ).named
        inputs.append(
            (
                int(vars["sx"]) + int(vars["sy"]) * 1j,
                int(vars["bx"]) + int(vars["by"]) * 1j,
            )
        )

    max_range = 4000000

    for inp in inputs:
        dis_bea = int(get_man_dis(inp[0], inp[1]))
        for p in list_man_dis_points(inp[0], dis_bea + 1, 0, max_range):
            for inp2 in inputs:
                if get_man_dis(inp2[0], p) <= get_man_dis(inp2[0], inp2[1]):
                    break
            else:
                return p.real * 4000000 + p.imag


def d15p2c() -> int:
    with open("day15input.txt") as f:
        lines = f.readlines()

    inputs = []

    for line in lines:
        vars = parse(
            "Sensor at x={sx}, y={sy}: closest beacon is at x={bx}, y={by}",
            line.strip(),
        ).named
        inputs.append(
            (
                int(vars["sx"]) + int(vars["sy"]) * 1j,
                int(vars["bx"]) + int(vars["by"]) * 1j,
            )
        )

    max_range = 4000000

    for inp1 in inputs:
        for inp2 in inputs:
            if (
                get_man_dis(inp1[0], inp2[0])
                == get_man_dis(inp1[0], inp1[1]) + get_man_dis(inp2[0], inp2[1]) + 2
            ):
                dis_bea = int(get_man_dis(inp1[0], inp1[1]))
                before_set = time.time()
                shared_border = set(
                    list_man_dis_points(
                        inp1[0], int(get_man_dis(inp1[0], inp1[1])) + 1, 0, max_range
                    )
                ) & set(
                    list_man_dis_points(
                        inp2[0], int(get_man_dis(inp2[0], inp2[1])) + 1, 0, max_range
                    )
                )
                after_set = time.time()
                for p in shared_border:
                    for inp3 in inputs:
                        if get_man_dis(inp3[0], p) <= get_man_dis(inp3[0], inp3[1]):
                            break
                    else:
                        after_loop = time.time()
                        print(f"Time to create set: {after_set - before_set}s")
                        print(f"Time loop over set: {after_loop - after_set}s")
                        return p.real * 4000000 + p.imag


print(d15p1())
# print(d15p2())
start = time.time()
print(d15p2b())
mid = time.time()
print(d15p2c())
end = time.time()

print(f"Interval method took {mid-start}s")
print(f"Border method took {end-mid}s")
