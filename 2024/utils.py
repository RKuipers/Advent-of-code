from heapq import heappop, heappush

MAXVAL = 2**31


class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, o):
        return Coord(self.x + o.x, self.y + o.y)

    def __sub__(self, o):
        return Coord(self.x - o.x, self.y - o.y)

    def __mul__(self, s):
        if isinstance(s, int) or isinstance(s, float):
            return Coord(self.x * s, self.y * s)
        elif isinstance(s, Coord):
            return Coord(self.x * s.x, self.y * s.y)

    def __eq__(self, o):
        return self.x == o.x and self.y == o.y

    def __lt__(self, o):
        return self.x < o.x and self.y < o.y

    def __gt__(self, o):
        return self.x > o.x and self.y > o.y

    def __le__(self, o):
        return self.x <= o.x and self.y <= o.y

    def __ge__(self, o):
        return self.x >= o.x and self.y >= o.y

    def __hash__(self):
        return hash(str(self))

    def __str__(self):
        return "Coord(%d, %d)" % (self.x, self.y)

    def __repr__(self):
        return "Coord(%d, %d)" % (self.x, self.y)

    def man_dis(self, o):
        return abs(self.x - o.x) + abs(self.y - o.y)


DIRS = [Coord(1, 0), Coord(-1, 0), Coord(0, -1), Coord(0, 1)]


def BFS(root, graph, is_terminal, at_terminal, one_terminal, get_neighbors, set_parent):
    queue = [root]
    explored = [root]
    while len(queue) > 0:
        v = queue.pop(0)
        if is_terminal(v, graph):
            at_terminal(v, graph)
            if one_terminal:
                return v
            else:
                continue
        pot_neighbors = get_neighbors(v, graph)
        neighbors = [w for w in pot_neighbors if not w in explored and not w in queue]
        for n in neighbors:
            set_parent(n, v)
            explored.append(n)
            queue.append(n)


def DFS(v, graph, is_terminal, at_terminal, get_neighbors, discover, is_discovered):
    discover(v, graph)

    if is_terminal(v, graph):
        at_terminal(v, graph)
        return [v]

    neighbors = get_neighbors(v, graph)
    neighbor_results = [
        DFS(n, graph, is_terminal, at_terminal, get_neighbors, discover, is_discovered)
        for n in neighbors
        if not is_discovered(n, graph)
    ]
    return [r for n in neighbor_results for r in n]


def a_star(
    start,
    goal,
    is_goal,
    heuristic,
    get_neighbors,
    grid,
    return_all=False,
    max_score=MAXVAL,
):
    def reconstruct_one(cameFrom, current):
        total_path = [current]
        while current in cameFrom.keys():
            current = cameFrom[current]
            total_path.append(current)
        return total_path

    def reconstruct_all(cameFrom, current):
        total_path = [current]
        currents = [current]
        while len(currents) > 0:
            current = currents.pop()
            if current in cameFrom:
                prevs = cameFrom[current]
                total_path = total_path + prevs
                currents = currents + prevs
        return total_path

    openset = []
    heappush(openset, (0, start))
    cameFrom = {}
    gScore = {start: 0}

    while len(openset) > 0:
        curr_fScore, current = heappop(openset)
        if is_goal(current):
            if not return_all:
                return gScore[current], reconstruct_one(cameFrom, current)
            else:
                return gScore[current], reconstruct_all(cameFrom, current)
        neighbors = get_neighbors(current, grid)
        for n in neighbors:
            tentative_gScore = gScore[current] + 1
            if tentative_gScore > max_score:
                continue
            if gScore.get(n, MAXVAL) > tentative_gScore:
                if return_all:
                    cameFrom[n] = [current]
                else:
                    cameFrom[n] = current
                gScore[n] = tentative_gScore
                if not n in {x[1] for x in openset}:
                    heappush(openset, (tentative_gScore + heuristic(n, goal), n))
            elif gScore.get(n, MAXVAL) == tentative_gScore and return_all:
                cameFrom[n].append(current)

    return MAXVAL, []


class Counter:
    def __init__(self):
        self.count = 0

    def count_one(self):
        self.count += 1

    def count_x(self, x):
        self.count += x
