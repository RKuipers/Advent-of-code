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