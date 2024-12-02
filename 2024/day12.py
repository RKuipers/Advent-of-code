import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d12a(parsed):
    pass

def d12b(parsed):
    pass

testfile = "2024/inputs/day12testinput.txt"
file = "2024/inputs/day12input.txt"

print("TEST:")
start = time.time()
print(d12a(parseA(testfile)))
mid = time.time()
print(d12b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d12a(parseA(file)))
mid = time.time()
print(d12b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")