import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d4a(parsed):
    pass

def d4b(parsed):
    pass

testfile = "2024/inputs/day4testinput.txt"
file = "2024/inputs/day4input.txt"

print("TEST:")
start = time.time()
print(d4a(parseA(testfile)))
mid = time.time()
print(d4b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d4a(parseA(file)))
mid = time.time()
print(d4b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")