n = input('');
m = input('');
degree = zeros(1, n);
for i = 1:m
    edge = input('');
    u = edge(1);
    v = edge(2);
    degree(u)++;
    degree(v)++;
end
fprintf('%d', degree(1));
for i = 2:n
    fprintf(' %d', degree(i));
end
fprintf('\n');
