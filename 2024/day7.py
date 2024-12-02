import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d7a(parsed):
    pass

def d7b(parsed):
    pass

testfile = "2024/inputs/day7testinput.txt"
file = "2024/inputs/day7input.txt"

print("TEST:")
start = time.time()
print(d7a(parseA(testfile)))
mid = time.time()
print(d7b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d7a(parseA(file)))
mid = time.time()
print(d7b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")