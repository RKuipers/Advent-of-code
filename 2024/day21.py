import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d21a(parsed):
    pass

def d21b(parsed):
    pass

testfile = "2024/inputs/day21testinput.txt"
file = "2024/inputs/day21input.txt"

print("TEST:")
start = time.time()
print(d21a(parseA(testfile)))
mid = time.time()
print(d21b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d21a(parseA(file)))
mid = time.time()
print(d21b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")