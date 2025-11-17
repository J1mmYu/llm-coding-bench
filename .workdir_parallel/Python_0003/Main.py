n = int(input())
arr = list(map(int, input().split()))

prefix_sum = 0
sum_index = {0: -1}
max_length = 0

for i in range(n):
    prefix_sum += arr[i]
    
    if prefix_sum in sum_index:
        max_length = max(max_length, i - sum_index[prefix_sum])
    else:
        sum_index[prefix_sum] = i

print(max_length)
