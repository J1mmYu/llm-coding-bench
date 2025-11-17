n = int(input())
arr = list(map(int, input().split()))

dp = []
for num in arr:
    left, right = 0, len(dp)
    while left < right:
        mid = (left + right) // 2
        if dp[mid] < num:
            left = mid + 1
        else:
            right = mid
    if left == len(dp):
        dp.append(num)
    else:
        dp[left] = num

print(len(dp))
