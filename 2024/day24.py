import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d24a(parsed):
    pass

def d24b(parsed):
    pass

testfile = "2024/inputs/day24testinput.txt"
file = "2024/inputs/day24input.txt"

print("TEST:")
start = time.time()
print(d24a(parseA(testfile)))
mid = time.time()
print(d24b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d24a(parseA(file)))
mid = time.time()
print(d24b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")