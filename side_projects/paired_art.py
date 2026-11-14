from wordfreq import top_n_list
import string

words = top_n_list("en", 30000)

# Script that takes a word and then gives all the valid words with 1 letter added to or removed from it


def run():
    print("Give me a word:")
    base_word = input().lower()

    if base_word not in words:
        print("[WARN] This word is not in the word list!")

    shorter = [base_word[:i] + base_word[i + 1 :] for i in range(len(base_word))]
    longer = [
        base_word[:i] + l + base_word[i:]
        for i in range(len(base_word) + 1)
        for l in string.ascii_lowercase
    ]

    shorter_words = set([word for word in shorter if word in words])
    longer_words = set([word for word in longer if word in words])

    print("")
    print("\n".join(shorter_words))
    print("")
    print("\n".join(longer_words))
    print("\n")

    run()


run()
