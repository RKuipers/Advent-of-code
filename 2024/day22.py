import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d22a(parsed):
    pass

def d22b(parsed):
    pass

testfile = "2024/inputs/day22testinput.txt"
file = "2024/inputs/day22input.txt"

print("TEST:")
start = time.time()
print(d22a(parseA(testfile)))
mid = time.time()
print(d22b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d22a(parseA(file)))
mid = time.time()
print(d22b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")