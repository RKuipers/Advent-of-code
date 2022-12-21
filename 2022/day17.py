rocks = [[2, 3, 4, 5], [2+1j, 3+1j, 4+1j, 3, 3+2j], [2, 3, 4, 4+1j, 4+2j], [2, 2+1j, 2+2j, 2+3j], [2, 2+1j, 3, 3+1j]]

def check_col(filled, width, rock):
    for r in rock:
        if r in filled[-300:] or r.real >= width or r.real < 0 or r.imag <= 0:
            return True
    return False

def d17p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
    winds = lines[0].strip()
    w = 0
    
    width = 7        
    heigth = 0
    
    filled = []
    
    for i in range(2022):
        rock = list(map(lambda x : x + (4+heigth) * 1j, rocks[i%len(rocks)]))
        while True:
            c = winds[w]
            w = (w + 1) % len(winds)
            if c == '<':
                new_rock = list(map(lambda x : x - 1, rock))
            elif c == '>':
                new_rock = list(map(lambda x : x + 1, rock))
                
            if not check_col(filled, width, new_rock):
                rock = new_rock
                
            new_rock = list(map(lambda x : x - 1j, rock))
            if check_col(filled, width, new_rock):
                break
            rock = new_rock
        heigth =  max(heigth, max([r.imag for r in rock]))
        filled.extend(rock)
        
    return heigth
        
def d17p2(file) -> int:
    def get_surface_state(rock, heigth, height_diff, state):
        if state == None:
            state = [heigth for _ in range(7)]
        else:
            state = list(map(lambda x : x + height_diff, state))
        
        for r in rock:
            state[int(r.real)] = min(state[int(r.real)], heigth - r.imag)
        
        return state
    
    with open(file) as f:
        lines = f.readlines()
    winds = lines[0].strip()
    w = 0
    
    width = 7        
    heigth = 0
    
    filled = []
    
    states = []
    heigths = []
    surface_state = None
    pred = None
    
    i = 0
    rounds = 1000000000000
    do_states = True
    while i < rounds:        
        rock = list(map(lambda x : x + (4+heigth) * 1j, rocks[i%len(rocks)]))
        while True:
            c = winds[w]
            w = (w + 1) % len(winds)
            if c == '<':
                new_rock = list(map(lambda x : x - 1, rock))
            elif c == '>':
                new_rock = list(map(lambda x : x + 1, rock))
                
            if not check_col(filled, width, new_rock):
                rock = new_rock
                
            new_rock = list(map(lambda x : x - 1j, rock))
            if check_col(filled, width, new_rock):
                break
            rock = new_rock
              
        old_heigth = heigth
        heigth = max(heigth, max([r.imag for r in rock]))    
            
        if do_states:        
            surface_state = get_surface_state(rock, heigth, heigth - old_heigth, surface_state)
            state = (w, i%5, surface_state)
            if state in states:
                cycle_length = i - states.index(state)
                cycles = int((rounds - i) / cycle_length)
                rounds = rounds - cycles * cycle_length
                
                heigth_diff = heigth - heigths[states.index(state)]
                heigth_bonus = cycles * heigth_diff
                
                do_states = False
        
        if do_states:
            states.append(state)
            heigths.append(heigth)
        
        filled.extend(rock)
        
        i = i + 1            
        
    return heigth + heigth_bonus
   
file = "day17input.txt"
print(d17p1(file))
print(d17p2(file))