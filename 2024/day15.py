import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d15a(parsed):
    pass

def d15b(parsed):
    pass

testfile = "2024/inputs/day15testinput.txt"
file = "2024/inputs/day15input.txt"

print("TEST:")
start = time.time()
print(d15a(parseA(testfile)))
mid = time.time()
print(d15b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d15a(parseA(file)))
mid = time.time()
print(d15b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")