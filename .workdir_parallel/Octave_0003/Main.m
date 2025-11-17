n = input('');
arr = input('');

prefix_sum = 0;
map = containers.Map('KeyType', 'double', 'ValueType', 'double');
map(0) = 0;
max_len = 0;

for i = 1:n
    prefix_sum = prefix_sum + arr(i);
    
    if isKey(map, prefix_sum)
        max_len = max(max_len, i - map(prefix_sum));
    else
        map(prefix_sum) = i;
    end
end

disp(max_len);
