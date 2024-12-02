import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d8a(parsed):
    pass

def d8b(parsed):
    pass

testfile = "2024/inputs/day8testinput.txt"
file = "2024/inputs/day8input.txt"

print("TEST:")
start = time.time()
print(d8a(parseA(testfile)))
mid = time.time()
print(d8b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d8a(parseA(file)))
mid = time.time()
print(d8b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")