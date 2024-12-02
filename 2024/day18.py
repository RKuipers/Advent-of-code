import time

def parseA(file):
    pass

def parseB(file):
    return parseA(file)

def d18a(parsed):
    pass

def d18b(parsed):
    pass

testfile = "2024/inputs/day18testinput.txt"
file = "2024/inputs/day18input.txt"

print("TEST:")
start = time.time()
print(d18a(parseA(testfile)))
mid = time.time()
print(d18b(parseB(testfile)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")

print("REAL:")
start = time.time()
print(d18a(parseA(file)))
mid = time.time()
print(d18b(parseB(file)))
end = time.time()

print(f"Part A took {mid-start}s")
print(f"Part B took {end-mid}s")