import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d17a(parsed):
    pass

def d17b(parsed):
    pass

testfile = "2024/inputs/day17testinput.txt"
file = "2024/inputs/day17input.txt"

print("TEST:")
start = time.time()
print(d17a(parseA(testfile)))
mid = time.time()
print(d17b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d17a(parseA(file)))
mid = time.time()
print(d17b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")