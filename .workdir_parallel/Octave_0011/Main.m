[n m] = scanf("%d %d", "C");
grid = cell(n, 1);
for i = 1:n
    grid{i} = strtrim(fgets(stdin));
end

visited = false(n, m);
count = 0;

function dfs(i, j, grid, visited, n, m)
    if i < 1 || i > n || j < 1 || j > m || visited(i, j) || grid{i}(j) == '#'
        return;
    end
    visited(i, j) = true;
    dfs(i+1, j, grid, visited, n, m);
    dfs(i-1, j, grid, visited, n, m);
    dfs(i, j+1, grid, visited, n, m);
    dfs(i, j-1, grid, visited, n, m);
end

for i = 1:n
    for j = 1:m
        if !visited(i, j) && grid{i}(j) == '.'
            stack = [i j];
            visited(i, j) = true;
            while size(stack, 1) > 0
                ci = stack(end, 1);
                cj = stack(end, 2);
                stack(end, :) = [];
                
                dirs = [1 0; -1 0; 0 1; 0 -1];
                for d = 1:4
                    ni = ci + dirs(d, 1);
                    nj = cj + dirs(d, 2);
                    if ni >= 1 && ni <= n && nj >= 1 && nj <= m && !visited(ni, nj) && grid{ni}(nj) == '.'
                        visited(ni, nj) = true;
                        stack = [stack; ni nj];
                    end
                end
            end
            count++;
        end
    end
end

printf("%d\n", count);
