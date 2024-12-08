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
