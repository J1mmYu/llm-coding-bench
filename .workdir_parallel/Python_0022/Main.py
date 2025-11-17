s = input()
char_index = {}
max_length = 0
start = 0

for i, char in enumerate(s):
    if char in char_index and char_index[char] >= start:
        start = char_index[char] + 1
    char_index[char] = i
    max_length = max(max_length, i - start + 1)

print(max_length)
