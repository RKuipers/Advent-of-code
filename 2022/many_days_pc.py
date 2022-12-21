import numpy as np

def process_row(row, bools):
    def process_one_way(row, bools):
        highest = -1
        for i in range(len(row)):
            if int(row[i]) > highest:
                highest = int(row[i])
                bools[i] = True
        return bools
    
    bools = process_one_way(row, bools)
    bools.reverse()
    row.reverse()
    bools = process_one_way(row, bools)
    bools.reverse()
    row.reverse()
    return bools

def d8p1() -> int:
    with open("day8input.txt") as f:
        lines = f.readlines()
        
    bool_field = []
    str_field = []    
        
    for l in lines:
        as_list = list(l)[:-1]
        str_field.append(as_list)
        bools = [False] * len(as_list)
        bools = process_row(as_list, bools)
        bool_field.append(bools)
        
    bool_field = np.transpose(bool_field)
    str_field = np.transpose(str_field)
    
    for i in range(len(str_field)):
        bools = process_row(list(str_field[i]), list(bool_field[i]))
        bool_field[i] = bools
        
    bool_field = [list(row) for row in bool_field]
        
    return sum([bool_field[i].count(True) for i in range(len(bool_field))])

def d8p2() -> int:
    with open("day8input.txt") as f:
        lines = f.readlines()
        
    results = []
        
    for y in range(len(lines)):
        for x in range(len(lines[y][:-1])):            
            height = int(lines[y][x])
            u = 0
            for y_ in range(1, y+1):
                if int(lines[y - y_][x]) >= height:
                    u = u + 1
                    break
                else:
                    u = u + 1
            d = 0
            for y_ in range(y+1, len(lines)):
                if int(lines[y_][x]) >= height:
                    d = d + 1
                    break
                else:
                    d = d + 1
            l = 0
            for x_ in range(1, x+1):
                if int(lines[y][x - x_]) >= height:
                    l = l + 1
                    break
                else:
                    l = l + 1
            r = 0
            for x_ in range(x+1, len(lines[y][:-1])):
                if int(lines[y][x_]) >= height:
                    r = r + 1
                    break
                else:
                    r = r + 1
            
            results.append(u*d*l*r)
            
    return max(results)

def update_tail(H, T):
    if abs(H[0] - T[0]) <= 1 and abs(H[1] - T[1]) <= 1:
        return T
    
    T = [(T[0] + H[0])/2, (T[1] + H[1])/2]
    
    return [T[i] if T[i] == int(T[i]) else H[i] for i in [0, 1]]

def d9p1() -> int:    
    with open("day9input.txt") as f:
        lines = f.readlines()
        
    H = [0, 0]
    T = [0, 0]
    
    T_set = set()
    T_set.add((T[0], T[1]))
    
    for line in lines:
        splt = line.split(" ")
        for _ in range(int(splt[1])):
            if splt[0] == "R":
                H[0] = H[0] + 1
            elif splt[0] == "L":
                H[0] = H[0] - 1
            elif splt[0] == "U":
                H[1] = H[1] + 1
            elif splt[0] == "D":
                H[1] = H[1] - 1
            T = update_tail(H, T)
            T_set.add((T[0], T[1]))
            
    return len(T_set)

def d9p2() -> int:    
    with open("day9input.txt") as f:
        lines = f.readlines()
        
    segments = [[0, 0] for _ in range(10)]
    
    T_set = set()
    T_set.add((0, 0))
    
    for line in lines:
        splt = line.split(" ")
        for _ in range(int(splt[1])):
            if splt[0] == "R":
                segments[0][0] = segments[0][0] + 1
            elif splt[0] == "L":
                segments[0][0] = segments[0][0] - 1
            elif splt[0] == "U":
                segments[0][1] = segments[0][1] + 1
            elif splt[0] == "D":
                segments[0][1] = segments[0][1] - 1
            
            for i in range(1, len(segments)):
                segments[i] = update_tail(segments[i-1], segments[i])
            
            T_set.add((segments[-1][0], segments[-1][1]))
            
    return len(T_set)

def get_signal(lines):
    x = 1
    signal = []
    
    for l in lines:
        if l[:4] == "noop":
            signal.append(x)
        else:
            signal.append(x)
            signal.append(x)
            splt = l.split(" ")
            x = x + int(splt[1])
            
    return signal

def d10p1() -> int:    
    with open("day10input.txt") as f:
        lines = f.readlines()
        
    signal = get_signal(lines)
            
    return sum([signal[i] * (i+1) for i in range(19, 220, 40)])
        
def d10p2() -> str:    
    with open("day10input.txt") as f:
        lines = f.readlines()
        
    signal = get_signal(lines)
    
    output = []
    
    for i in range(6):
        output.append("".join(["#" if abs(signal[i*40+j] - j) <= 1 else "." for j in range(40)]))
        
    return "\n".join(output)

import re

def d11p1() -> int:    
    with open("day11input.txt") as f:
        lines = f.readlines()
        
    items = []
    y_vals = []
    tests = []
    
    ops = {}
    ys = {}
        
    inspect = lambda m, x : ops[m](x, ys[m](m, x))
    test = lambda m, x : tests[m][1] if x % tests[m][0] == 0 else tests[m][2]

    active_test = []
    
    monkey = 0
    counts = []
        
    for l in lines:
        splt = l.split()
        if len(splt) == 0:
            continue
        elif splt[0] == "Monkey":
            monkey = int(splt[1][:-1])
            counts.append(0)
        elif splt[0] == "Starting":
            items.append([int(i) for i in re.split(' |, |\n', l)[4:-1]])
        elif splt[0] == "Operation:":
            y_vals.append(int(splt[-1]) if splt[-1] != "old" else -1)
            ops.update({monkey: (lambda x, y : x + y) if splt[4] == "+" else (lambda x, y : x * y)})
            ys.update({monkey: (lambda m, x : x) if splt[-1] == "old" else (lambda m, x : y_vals[m])})
        elif splt[0] == "Test:" or splt[0] == "If":
            active_test.append(int(splt[-1]))
            if splt[1] == "false:":
                tests.append((active_test[0], active_test[1], active_test[2]))
                active_test = []
    
    for r in range(0, 20):
        for m in range(len(items)):
            counts[m] = counts[m] + len(items[m])          
            for i in items[m]:
                v = int(inspect(m, i) / 3)
                items[test(m, v)].append(v)
            items[m] = []
                
    counts.sort()
    return counts[-1] * counts[-2]
            

def d11p2() -> int:    
    with open("day11input.txt") as f:
        lines = f.readlines()
        
    items = []
    y_vals = []
    tests = []
    
    ops = {}
    ys = {}
        
    inspect = lambda m, x : ops[m](x, ys[m](m, x))
    test = lambda m, x : tests[m][1] if x % tests[m][0] == 0 else tests[m][2]

    active_test = []
    
    monkey = 0
    counts = []
        
    for l in lines:
        splt = l.split()
        if len(splt) == 0:
            continue
        elif splt[0] == "Monkey":
            monkey = int(splt[1][:-1])
            counts.append(0)
        elif splt[0] == "Starting":
            items.append([int(i) for i in re.split(' |, |\n', l)[4:-1]])
        elif splt[0] == "Operation:":
            y_vals.append(int(splt[-1]) if splt[-1] != "old" else -1)
            ops.update({monkey: (lambda x, y : np.int64(x) + np.int64(y)) if splt[4] == "+" else (lambda x, y : np.int64(x) * np.int64(y))})
            ys.update({monkey: (lambda m, x : x) if splt[-1] == "old" else (lambda m, x : y_vals[m])})
        elif splt[0] == "Test:" or splt[0] == "If":
            active_test.append(int(splt[-1]))
            if splt[1] == "false:":
                tests.append((active_test[0], active_test[1], active_test[2]))
                active_test = []
    
    mod = np.prod([vals[0] for vals in tests])
    
    for r in range(0, 10000):
        for m in range(len(items)):
            counts[m] = counts[m] + len(items[m])          
            for i in items[m]:
                v = inspect(m, i) % mod
                items[test(m, v)].append(v)
            items[m] = []
                
    counts.sort()
    return counts[-1] * counts[-2]

import ast

def compare(left, right) -> int:
    if isinstance(left, int) and isinstance(right, int):
        if left == right:
            return 0
        else: 
            return -1 if left < right else 1
    if isinstance(left, list) and isinstance(right, list):
        for l, r in zip(left, right):
            comp = compare(l, r)
            if comp != 0:
                return comp
        return compare(len(left), len(right))                
    to_list = lambda x : x if isinstance(x, list) else [x]
    return compare(to_list(left), to_list(right))

def d13p1() -> int:    
    with open("day13input.txt") as f:
        lines = f.readlines()
        
    right_order = []    
        
    for i in range(0, len(lines), 3):
        left = ast.literal_eval(lines[i])
        right = ast.literal_eval(lines[i+1])
        if compare(left, right) == -1:
            right_order.append((i / 3) + 1)
            
    return sum(right_order)

import functools    

def d13p2() -> int:
    with open("day13input.txt") as f:
        lines = f.readlines()
        
    divs = [[[2]], [[6]]]
    
    lines = list(filter(("\n").__ne__, lines))
    
    elements = list(map(ast.literal_eval, lines))   
    elements.extend(divs) 
    elements.sort(key=functools.cmp_to_key(compare))
    
    return np.prod([elements.index(x) + 1 for x in divs])

import itertools

def get_next(filled, x, y):
    if y+1 not in filled.keys():
        return x
    if x not in filled[y+1]:
        return x
    elif x-1 not in filled[y+1]:
        return x-1
    elif x+1 not in filled[y+1]:
        return x+1
    return None

def d14p1() -> int:
    with open("day14inputb.txt") as f:
        lines = f.readlines()
        
    filled = {}    
        
    for line in lines:
        splt = line[:-1].split(" -> ")
        for i, segment in enumerate(splt[:-1]):
            x1, y1 = map(int, segment.split(","))
            x2, y2 = map(int, splt[i+1].split(","))
            for x, y in [(x, y) for x in range(min(x1, x2), max(x1, x2)+1) for y in range(min(y1, y2), max(y1, y2)+1)]:
                if y in filled.keys():
                    filled[y].add(x)
                else:
                    filled.update({y: {x}})
                    
    origin = (0, 500)
    heigth = max(filled.keys())
    
    for i in itertools.count():
        n = origin[1]
        y = origin[0] - 1
        while n is not None and y < heigth:
            x = n
            y = y + 1
            n = get_next(filled, x, y)
        if y == heigth:
            result = i
            break
        if y in filled.keys():
            filled[y].add(x)
        else:
            filled.update({y: {x}})
        
    return result
        
def d14p2() -> int:
    with open("day14input.txt") as f:
        lines = f.readlines()
        
    filled = {}    
        
    for line in lines:
        splt = line[:-1].split(" -> ")
        for i, segment in enumerate(splt[:-1]):
            x1, y1 = map(int, segment.split(","))
            x2, y2 = map(int, splt[i+1].split(","))
            for x, y in [(x, y) for x in range(min(x1, x2), max(x1, x2)+1) for y in range(min(y1, y2), max(y1, y2)+1)]:
                if y in filled.keys():
                    filled[y].add(x)
                else:
                    filled.update({y: {x}})
                    
    origin = (0, 500)
    heigth = max(filled.keys())+1
    
    for i in itertools.count():        
        n = origin[1]
        y = origin[0]-1
        while n is not None and y < heigth:
            x = n
            y = y + 1
            n = get_next(filled, x, y)
        if y == origin[0]:
            result = i+1
            break
        if y in filled.keys():
            filled[y].add(x)
        else:
            filled.update({y: {x}})
        
    return result

def d14p2b() -> int:
    with open("day14input.txt") as f:
        lines = f.readlines()
        
    filled = {}    
        
    for line in lines:
        splt = line[:-1].split(" -> ")
        for i, segment in enumerate(splt[:-1]):
            x1, y1 = map(int, segment.split(","))
            x2, y2 = map(int, splt[i+1].split(","))
            for x, y in [(x, y) for x in range(min(x1, x2), max(x1, x2)+1) for y in range(min(y1, y2), max(y1, y2)+1)]:
                if y in filled.keys():
                    filled[y].add(x)
                else:
                    filled.update({y: {x}})
    
    heigth = max(filled.keys())+2
                    
    for y in range(0, heigth):
        for x in range(500-y, 501+y):            
            if (y not in filled.keys() or x not in filled[y]) and y-1 in filled.keys() and {x-1, x, x+1}.issubset(filled[y-1]):
                if y in filled.keys():
                    filled[y].add(x)
                else:
                    filled.update({y: {x}})
                
    return heigth**2 - sum([len(row) for row in filled.values()])

from parse import parse
import mip as mip

def d16p1() -> int:
    with open("day16input.txt") as f:
        lines = f.readlines()
        
    max_dur = 30    
    start_name = "AA"
        
    model = mip.Model("d16p1")
    obj = mip.LinExpr(0)
    vars_by_min = [{} for _ in range(max_dur)]
        
    for l in lines:
        if "," in l:
            vals = parse("Valve {n} has flow rate={r}; tunnels lead to valves {e}\n", l).named
        else:
            vals = parse("Valve {n} has flow rate={r}; tunnel leads to valve {e}\n", l).named
        name = vals["n"]
        rate = int(vals["r"])
        edges = vals["e"].split(", ")
        
        valve_const = mip.LinExpr(0)  
        
        for m in range(max_dur):
            lb = 1 if m == 0 and name == start_name else 0                        
            vars_by_min[m][name] = model.add_var(f"{name}_valve_{m}", var_type=mip.BINARY, lb=lb)  
            obj.add_var(vars_by_min[m][name], rate * (max_dur - m))
            valve_const.add_var(vars_by_min[m][name], 1)
            for e in edges:
                if f"{e}_{name}" not in vars_by_min[m].keys():
                    vars_by_min[m][f"{e}_{name}"] = model.add_var(f"{e}_{name}_edge_{m}", var_type=mip.BINARY)
                    vars_by_min[m][f"{name}_{e}"] = model.add_var(f"{name}_{e}_edge_{m}", var_type=mip.BINARY)                
                    
            if m > 0:
                outgoing = mip.LinExpr(variables=[vars_by_min[m][name]], coeffs=[1])
                incoming = mip.LinExpr(variables=[vars_by_min[m-1][name]], coeffs=[1])
                for e in edges:
                    outgoing.add_var(vars_by_min[m][f"{name}_{e}"], 1)
                    incoming.add_var(vars_by_min[m-1][f"{e}_{name}"], 1)
                model.add_constr(incoming <= outgoing)
        
        model.add_constr(valve_const <= 1)
    
    for m in vars_by_min: 
        this_min_total = mip.LinExpr(0)
        for v in m.values():
            this_min_total.add_var(v, 1)
        model.add_constr(this_min_total <= 1)
                
    model.objective = mip.maximize(obj)
    
    model.write("d16p1.lp")
    
    model.optimize()
    
    selected = {m: k for m in range(max_dur) for k, v in vars_by_min[m].items() if v.x >= 0.99}
    
    return model.objective_value

def d16p2() -> int:
    with open("day16input.txt") as f:
        lines = f.readlines()
        
    max_dur = 26    
    start_name = "AA"
        
    model = mip.Model("d16p2")
    obj = mip.LinExpr(0)
    vars_by_min = [{} for _ in range(max_dur)]
    vars_by_min_ele = [{} for _ in range(max_dur)]
        
    for l in lines:
        if "," in l:
            vals = parse("Valve {n} has flow rate={r}; tunnels lead to valves {e}\n", l).named
        else:
            vals = parse("Valve {n} has flow rate={r}; tunnel leads to valve {e}\n", l).named
        name = vals["n"]
        rate = int(vals["r"])
        edges = vals["e"].split(", ")
        
        valve_const = mip.LinExpr(0)  
        
        for m in range(max_dur):
            lb = 1 if m == 0 and name == start_name else 0                        
            vars_by_min[m][name] = model.add_var(f"{name}_valve_{m}", var_type=mip.BINARY, lb=lb)  
            vars_by_min_ele[m][name] = model.add_var(f"{name}_valve_{m}_ele", var_type=mip.BINARY, lb=lb) 
            obj.add_var(vars_by_min[m][name], rate * (max_dur - m))
            obj.add_var(vars_by_min_ele[m][name], rate * (max_dur - m))
            valve_const.add_var(vars_by_min[m][name], 1)
            valve_const.add_var(vars_by_min_ele[m][name], 1)
            for e in edges:
                if f"{e}_{name}" not in vars_by_min[m].keys():
                    vars_by_min[m][f"{e}_{name}"] = model.add_var(f"{e}_{name}_edge_{m}", var_type=mip.BINARY)
                    vars_by_min[m][f"{name}_{e}"] = model.add_var(f"{name}_{e}_edge_{m}", var_type=mip.BINARY)  
                    vars_by_min_ele[m][f"{e}_{name}"] = model.add_var(f"{e}_{name}_edge_{m}_ele", var_type=mip.BINARY)
                    vars_by_min_ele[m][f"{name}_{e}"] = model.add_var(f"{name}_{e}_edge_{m}_ele", var_type=mip.BINARY)               
                    
            if m > 0:
                outgoing = mip.LinExpr(variables=[vars_by_min[m][name]], coeffs=[1])
                incoming = mip.LinExpr(variables=[vars_by_min[m-1][name]], coeffs=[1])
                for e in edges:
                    outgoing.add_var(vars_by_min[m][f"{name}_{e}"], 1)
                    incoming.add_var(vars_by_min[m-1][f"{e}_{name}"], 1)
                model.add_constr(incoming <= outgoing)
                
                outgoing_ele = mip.LinExpr(variables=[vars_by_min_ele[m][name]], coeffs=[1])
                incoming_ele = mip.LinExpr(variables=[vars_by_min_ele[m-1][name]], coeffs=[1])
                for e in edges:
                    outgoing_ele.add_var(vars_by_min_ele[m][f"{name}_{e}"], 1)
                    incoming_ele.add_var(vars_by_min_ele[m-1][f"{e}_{name}"], 1)
                model.add_constr(incoming_ele <= outgoing_ele)
        
        if rate > 0:
            model.add_constr(valve_const <= 1)
    
    for m in vars_by_min: 
        this_min_total = mip.LinExpr(0)
        for v in m.values():
            this_min_total.add_var(v, 1)
        model.add_constr(this_min_total <= 1)
        
    for m in vars_by_min_ele: 
        this_min_total_ele = mip.LinExpr(0)
        for v in m.values():
            this_min_total_ele.add_var(v, 1)
        model.add_constr(this_min_total_ele <= 1)
                
    model.objective = mip.maximize(obj)
    
    model.write("d16p2.lp")
    
    status = model.optimize()
    
    selected = {m: k for m in range(max_dur) for k, v in vars_by_min[m].items() if v.x >= 0.99}
    selected_ele = {m: k for m in range(max_dur) for k, v in vars_by_min_ele[m].items() if v.x >= 0.99}
    
    print(selected)
    print(selected_ele)
    
    return model.objective_value
        

import time

start = time.perf_counter()
print(d16p1())
middle = time.perf_counter()
print(d16p2())
end = time.perf_counter()

print(f"Part 1 runtime: {middle-start}s")
print(f"Part 2 runtime: {end-middle}s")