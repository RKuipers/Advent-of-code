import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d3a(parsed):
    pass

def d3b(parsed):
    pass

testfile = "2024/inputs/day3testinput.txt"
file = "2024/inputs/day3input.txt"

print("TEST:")
start = time.time()
print(d3a(parseA(testfile)))
mid = time.time()
print(d3b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d3a(parseA(file)))
mid = time.time()
print(d3b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")