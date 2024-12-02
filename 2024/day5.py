import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d5a(parsed):
    pass

def d5b(parsed):
    pass

testfile = "2024/inputs/day5testinput.txt"
file = "2024/inputs/day5input.txt"

print("TEST:")
start = time.time()
print(d5a(parseA(testfile)))
mid = time.time()
print(d5b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d5a(parseA(file)))
mid = time.time()
print(d5b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")