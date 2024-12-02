import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d10a(parsed):
    pass

def d10b(parsed):
    pass

testfile = "2024/inputs/day10testinput.txt"
file = "2024/inputs/day10input.txt"

print("TEST:")
start = time.time()
print(d10a(parseA(testfile)))
mid = time.time()
print(d10b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d10a(parseA(file)))
mid = time.time()
print(d10b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")