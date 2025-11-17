n = input('');
intervals = zeros(n, 2);
for i = 1:n
    intervals(i, :) = input('');
end

if n == 0
    disp(0);
else
    intervals = sortrows(intervals, 1);
    
    merged = [];
    current_start = intervals(1, 1);
    current_end = intervals(1, 2);
    
    for i = 2:n
        if intervals(i, 1) <= current_end
            current_end = max(current_end, intervals(i, 2));
        else
            merged = [merged; current_start, current_end];
            current_start = intervals(i, 1);
            current_end = intervals(i, 2);
        end
    end
    merged = [merged; current_start, current_end];
    
    total_length = sum(merged(:, 2) - merged(:, 1));
    disp(total_length);
end
