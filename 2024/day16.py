import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d16a(parsed):
    pass

def d16b(parsed):
    pass

testfile = "2024/inputs/day16testinput.txt"
file = "2024/inputs/day16input.txt"

print("TEST:")
start = time.time()
print(d16a(parseA(testfile)))
mid = time.time()
print(d16b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d16a(parseA(file)))
mid = time.time()
print(d16b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")