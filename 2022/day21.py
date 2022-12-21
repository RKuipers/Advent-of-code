import time
import operator
from sympy import symbols, solve

ops = {"+": operator.add, "-": operator.sub, "*": operator.mul, "/":operator.truediv, "=": operator.eq}

def d21p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    d = {}    
        
    for l in lines:
        splt = l.strip().split(": ")
        if splt[1].isnumeric():
            d.update({splt[0]: int(splt[1])})
        else:
            d.update({splt[0]: splt[1].split(" ")})
            
    to_eval = ["root"]
    
    while len(to_eval) > 0:
        n = to_eval[-1]
        x = d[n]
        l = x[0]
        r = x[2]
        if type(d[l]) == type(1) and type(d[r]) == type(1):
            d[n] = int(ops[x[1]](d[l], d[r]))
            to_eval = to_eval[:-1]
            continue
        
        if type(d[l]) == type([]) and l not in to_eval:
            to_eval.append(l)
        if type(d[r]) == type([]) and r not in to_eval:
            to_eval.append(r)
    
    return d["root"]
        
def d21p2(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    d = {}    
        
    for l in lines:
        splt = l.strip().split(": ")   
        if splt[1].isnumeric():
            d.update({splt[0]: int(splt[1])})
        else:
            d.update({splt[0]: splt[1].split(" ")})
    
    d["root"][1] = "-"  
    d["humn"] = "humn"
    
    to_eval = ["root"]
    
    while len(to_eval) > 0:
        n = to_eval[-1]
        x = d[n]
        l = x[0]
        r = x[2]
        
        if type(d[l]) == type(1) and type(d[r]) == type(1):
            d[n] = int(ops[x[1]](d[l], d[r]))
            to_eval = to_eval[:-1]
            continue    
        
        if (type(d[l]) == type("") or type(d[l]) == type(1)) and (type(d[r]) == type(1) or type(d[r]) == type("")):
            d[n] = f"({d[l]} {x[1]} {d[r]})"
            to_eval = to_eval[:-1]
            continue   
        
        if type(d[l]) == type([]):
            to_eval.append(l)
        if type(d[r]) == type([]):
            to_eval.append(r)
    
    humn = symbols('humn')
    return solve(d["root"])
        
file = "day21input.txt"
start = time.time()
print(d21p1(file))
middle = time.time()
print(d21p2(file))
end = time.time()

print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")