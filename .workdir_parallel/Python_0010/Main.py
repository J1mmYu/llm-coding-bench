from collections import Counter

line = input()
words = line.split()
word_counts = Counter(words)

sorted_words = sorted(word_counts.items(), key=lambda x: (-x[1], x[0]))

for word, count in sorted_words:
    print(f"{word} {count}")
