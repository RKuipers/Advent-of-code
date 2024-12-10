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


class Counter:
    def __init__(self):
        self.count = 0

    def count_one(self):
        self.count += 1

    def count_x(self, x):
        self.count += x