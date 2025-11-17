n, m = map(int, input().split())
degrees = [0] * (n + 1)

for _ in range(m):
    u, v = map(int, input().split())
    degrees[u] += 1
    degrees[v] += 1

print(' '.join(map(str, degrees[1:n+1])))
