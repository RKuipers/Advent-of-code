import time
import re

numbers = {
    "one": 1, 
    "two": 2, 
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7, 
    "eight": 8,
    "nine": 9,
    }

def d1(file) -> int:
    with open(file) as f:
        lines = f.readlines()
        
    s = 0
    for line in lines:
        l = line.strip()
        a = re.search("\d|one|two|three|four|five|six|seven|eight|nine",l)[0]
        b = re.search("\d|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin",l[::-1])[0][::-1]
        a_int = numbers[a] if a in numbers.keys() else int(a)
        b_int = numbers[b] if b in numbers.keys() else int(b)
        s += a_int * 10 + b_int
    return s

day = 1
smallFile = f"inputs/small_{day}.txt"
largeFile = f"inputs/large_{day}.txt"
print(d1(smallFile))
start = time.time()
print(d1(largeFile))
middle = time.time()
#print(d1(file))
end = time.time()

print(f"Part 1 runs in {middle - start}s")
print(f"Part 2 runs in {end - middle}s")