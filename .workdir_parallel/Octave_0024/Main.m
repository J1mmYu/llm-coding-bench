n = input('');
m = input('');
deg = zeros(1, n);
for i = 1:m
    edge = input('');
    u = edge(1);
    v = edge(2);
    deg(u) = deg(u) + 1;
    deg(v) = deg(v) + 1;
end
fprintf('%d', deg(1));
for i = 2:n
    fprintf(' %d', deg(i));
end
fprintf('\n');
