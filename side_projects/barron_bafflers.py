from wordfreq import top_n_list

words = top_n_list("en", 30000)

word_3 = [w for w in words if len(w) == 3]
word_4 = [w for w in words if len(w) == 4]
word_6 = [w for w in words if len(w) == 6]
word_7 = [w for w in words if len(w) == 7]

print(f"Three letter words: {len(word_3)}")
print(f"Four letter words: {len(word_4)}")
print(f"Six letter words: {len(word_6)}")
print(f"Seven letter words: {len(word_7)}")

clue_1_options = [w for w in word_6 if w[2:5] in word_3]
clue_2_options = [
    w for w in word_7 if w[1:4] in word_3 and "m" in w and "a" in w and "t" in w
]
clue_3_options = [
    w for w in word_6 if w[0:3] in word_3 and "a" in w and "r" in w and "t" in w
]
clue_4_options = [
    w for w in word_6 if w[3:6] in word_3 and "n" in w and "o" in w and "t" in w
]
clue_5_options = [
    w for w in word_6 if w[3:6] in word_3 and "t" in w and "e" in w and "n" in w
]
clue_6_options = [
    w for w in word_6 if w[2:6] in word_4 and "i" in w and "c" in w and "e" in w
]

print(f"Clue 1 options: {len(clue_1_options)}")
print(f"Clue 2 options: {len(clue_2_options)}")
print(f"Clue 3 options: {len(clue_3_options)}")
print(f"Clue 4 options: {len(clue_4_options)}")
print(f"Clue 5 options: {len(clue_5_options)}")
print(f"Clue 6 options: {len(clue_6_options)}")

print("Done")
