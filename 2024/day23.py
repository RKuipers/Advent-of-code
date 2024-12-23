import time
from utils import Coord


def parseA(file):    
    with open(file) as f:
        lines = f.readlines()

    return [tuple(l.strip().split("-")) for l in lines]


def parseB(file):
    return parseA(file)


def d23a(edges):
    t_nodes = {}
    for e in edges:
        if e[0][0] == "t":
            if e[0] in t_nodes.keys():
                t_nodes[e[0]].append(e[1])
            else:
                t_nodes[e[0]] = [e[1]]
        elif e[1][0] == "t":
            if e[1] in t_nodes.keys():
                t_nodes[e[1]].append(e[0])
            else:
                t_nodes[e[1]] = [e[0]]
    
    count = 0
    for t_node, nodes in t_nodes.items():
        for i, n in enumerate(nodes):
            for j, n2 in enumerate(nodes[i:]):
                if (n, n2) in edges or (n2, n) in edges:
                    count += 1
    return count

def bron_kerbosch(r, p, x, neighbors):
    if len(p) == 0 and len(x) == 0:
        return r
    n_neighs = {u: len(neighbors[u]) for u in p.union(x)}
    u = min(n_neighs, key=n_neighs.get)
    nu = neighbors[u]
    max_clique = set()
    for v in p.difference(nu):
        nv = neighbors[v]
        new_clique = bron_kerbosch(r.union({v}), p.intersection(nv), x.intersection(nv), neighbors)
        if len(new_clique) > len(max_clique):
            max_clique = new_clique
        p.remove(v)
        x.add(v)
    return max_clique
    

def d23b(edges):
    neighbors = {}
    for e in edges:
        if e[0] in neighbors.keys():
            neighbors[e[0]].add(e[1])
        else:
            neighbors[e[0]] = {e[1]}        
        if e[1] in neighbors.keys():
            neighbors[e[1]].add(e[0])
        else:
            neighbors[e[1]] = {e[0]}
    max_clique = bron_kerbosch(set(), set(neighbors.keys()), set(), neighbors)
    return ",".join(sorted(list(max_clique)))


testfile = "2024/inputs/day23testinput.txt"
file = "2024/inputs/day23input.txt"

print("TEST:")
start = time.time()
print(d23a(parseA(testfile)))
mid = time.time()
print(d23b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d23a(parseA(file)))
mid = time.time()
print(d23b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")
