function topological_sort()
    input_data = input('', 's');
    lines = strsplit(input_data, '\n');
    
    first_line = str2num(lines{1});
    n = first_line(1);
    m = first_line(2);
    
    adj = cell(n, 1);
    for i = 1:n
        adj{i} = [];
    end
    
    indegree = zeros(n, 1);
    
    for i = 1:m
        edge = str2num(lines{i+1});
        u = edge(1);
        v = edge(2);
        adj{u} = [adj{u}, v];
        indegree(v) = indegree(v) + 1;
    end
    
    queue = [];
    for i = 1:n
        if indegree(i) == 0
            queue = [queue, i];
        end
    end
    
    result = [];
    
    while length(queue) > 0
        u = queue(1);
        queue = queue(2:end);
        result = [result, u];
        
        for i = 1:length(adj{u})
            v = adj{u}(i);
            indegree(v) = indegree(v) - 1;
            if indegree(v) == 0
                queue = [queue, v];
            end
        end
    end
    
    if length(result) != n
        disp(-1);
    else
        disp(result);
    end
end

topological_sort()
