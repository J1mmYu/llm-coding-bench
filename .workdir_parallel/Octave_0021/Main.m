n = input('');
values = [];
for i = 1:n
    values(i) = input('');
end
fprintf('%d\n', length(unique(values)));
