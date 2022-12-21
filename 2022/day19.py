import mip as mip
from parse import parse


def check_blueprint(mins, blueprint) -> float:
    model = mip.Model(sense=mip.MAXIMIZE)
    obj = mip.LinExpr(0)
    vars_all = []
    for m in range(mins):
        vars = {}
        vars.update({"ore": model.add_var(f"ore_{m}", var_type=mip.BINARY)})
        vars.update({"clay": model.add_var(f"clay_{m}", var_type=mip.BINARY)})
        vars.update({"obs": model.add_var(f"obs_{m}", var_type=mip.BINARY)})
        vars.update({"geo": model.add_var(f"geo_{m}", var_type=mip.BINARY)})
        vars_all.append(vars)

        model.add_constr(vars["ore"] + vars["clay"] + vars["obs"] + vars["geo"] <= 1)

        ore = mip.LinExpr(const=m)
        clay = mip.LinExpr(const=0)
        obs = mip.LinExpr(const=0)
        for m_ in range(m):
            ore.add_var(vars_all[m_]["ore"], m - m_ - 1)
            ore.add_var(vars_all[m_]["ore"], -blueprint["oror"])
            ore.add_var(vars_all[m_]["clay"], -blueprint["clor"])
            ore.add_var(vars_all[m_]["obs"], -blueprint["obor"])
            ore.add_var(vars_all[m_]["geo"], -blueprint["geor"])

            clay.add_var(vars_all[m_]["clay"], m - m_ - 1)
            clay.add_var(vars_all[m_]["obs"], -blueprint["obcl"])

            obs.add_var(vars_all[m_]["obs"], m - m_ - 1)
            obs.add_var(vars_all[m_]["geo"], -blueprint["geob"])

        model.add_constr(
            ore
            >= vars["ore"] * blueprint["oror"]
            + vars["clay"] * blueprint["clor"]
            + vars["obs"] * blueprint["obor"]
            + vars["geo"] * blueprint["geor"]
        )
        model.add_constr(clay >= vars["obs"] * blueprint["obcl"])
        model.add_constr(obs >= vars["geo"] * blueprint["geob"])

        obj.add_var(vars["geo"], mins - m - 1)

    model.objective = obj

    model.write(f"d19p1_{blueprint['i']}.lp")
    model.optimize()

    return model.objective_value


def d19p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()

    mins = 24
    blueprints = []
    score = []

    for l in lines:
        vars = parse(
            "Blueprint {i}: Each ore robot costs {oror} ore. Each clay robot costs {clor} ore. Each obsidian robot costs {obor} ore and {obcl} clay. Each geode robot costs {geor} ore and {geob} obsidian.",
            l.strip(),
        ).named
        blueprints.append({k: int(v) for k, v in vars.items()})

    for blueprint in blueprints:
        val = check_blueprint(mins, blueprint)
        score.append(val * blueprint["i"])

    return sum(score)


def d19p2(file) -> int:
    with open(file) as f:
        lines = f.readlines()

    mins = 32
    blueprints = []
    score = []

    for l in lines:
        vars = parse(
            "Blueprint {i}: Each ore robot costs {oror} ore. Each clay robot costs {clor} ore. Each obsidian robot costs {obor} ore and {obcl} clay. Each geode robot costs {geor} ore and {geob} obsidian.",
            l.strip(),
        ).named
        blueprints.append({k: int(v) for k, v in vars.items()})

    for blueprint in blueprints[:3]:
        val = check_blueprint(mins, blueprint)
        score.append(val)

    return score[0] * score[1] * score[2]


import time

file = "day19input.txt"
start = time.time()
print(d19p1(file))
mid = time.time()
print(d19p2(file))
end = time.time()

print(f"Part 1 took {mid-start}s")
print(f"Part 2 took {end-mid}s")
