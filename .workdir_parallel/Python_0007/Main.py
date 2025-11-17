s = input()
k = int(input())

smallest = s[:k]
for i in range(1, len(s) - k + 1):
    substring = s[i:i+k]
    if substring < smallest:
        smallest = substring

print(smallest)
