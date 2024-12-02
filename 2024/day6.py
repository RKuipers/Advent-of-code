import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d6a(parsed):
    pass

def d6b(parsed):
    pass

testfile = "2024/inputs/day6testinput.txt"
file = "2024/inputs/day6input.txt"

print("TEST:")
start = time.time()
print(d6a(parseA(testfile)))
mid = time.time()
print(d6b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d6a(parseA(file)))
mid = time.time()
print(d6b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")