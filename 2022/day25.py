import time
import math

def snafu_to_dec(snafu):
    def parse_chars(l):
        return [int(c) if c.isnumeric() else (-1 if c == "-" else -2) for c in l]
    
    parsed = parse_chars(snafu)
    parsed.reverse()
    return sum([5**i * v for i, v in enumerate(parsed)])

def dec_to_snafu(dec):
    def max_expressed(i):
        return sum([2 * 5**i for i in range(i)])
    def parse_to_snafu(l):
        return [str(d) if d >= 0 else ("-" if d == -1 else "=") for d in l]
    
    dec_remaining = dec
    digits = []
    n_digits = 0
    round_func = lambda x : round(x)
    
    while max_expressed(n_digits) < dec:
        n_digits = n_digits + 1
        
    for i_ in range(n_digits):
        i = n_digits - i_ - 1
        
        digits.append(int(round_func(dec_remaining / 5**i)))
        dec_remaining = dec_remaining - (digits[-1] * 5**i)
        
    return "".join(parse_to_snafu(digits))

def d25p1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    return dec_to_snafu(sum([snafu_to_dec(list(l.strip())) for l in lines]))
        
        
def d25p2(file) -> int:  
    with open(file) as f:
        lines = f.readlines() 
    
        
file = "day25input.txt"
start = time.time()
print(d25p1(file))
middle = time.time()
print(d25p2(file))
end = time.time()

print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")