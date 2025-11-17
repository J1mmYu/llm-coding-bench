def count_connected_components():
    n, m = map(int, input().split())
    grid = []
    for _ in range(n):
        grid.append(input().strip())
    
    visited = [[False] * m for _ in range(n)]
    count = 0
    
    def dfs(i, j):
        if i < 0 or i >= n or j < 0 or j >= m:
            return
        if visited[i][j] or grid[i][j] == '#':
            return
        
        visited[i][j] = True
        dfs(i+1, j)
        dfs(i-1, j)
        dfs(i, j+1)
        dfs(i, j-1)
    
    for i in range(n):
        for j in range(m):
            if grid[i][j] == '.' and not visited[i][j]:
                dfs(i, j)
                count += 1
    
    print(count)

count_connected_components()
