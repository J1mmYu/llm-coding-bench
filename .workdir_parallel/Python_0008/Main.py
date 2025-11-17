n = int(input())
intervals = []
for _ in range(n):
    l, r = map(int, input().split())
    intervals.append((l, r))

intervals.sort()

merged = []
for l, r in intervals:
    if merged and l <= merged[-1][1]:
        merged[-1] = (merged[-1][0], max(merged[-1][1], r))
    else:
        merged.append((l, r))

total_length = sum(r - l for l, r in merged)
print(total_length)
