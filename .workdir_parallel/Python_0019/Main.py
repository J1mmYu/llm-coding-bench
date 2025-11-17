n = int(input())

def factorial(k):
    if k == 0 or k == 1:
        return 1
    result = 1
    for i in range(2, k + 1):
        result *= i
    return result

result = -1
for k in range(1, 13):
    if factorial(k) % n == 0:
        result = k
        break

print(result)
