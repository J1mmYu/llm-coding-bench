s = input('', 's');
n = length(s);
maxLen = 0;

for i = 1:n
    seen = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    for j = i:n
        if isKey(seen, s(j))
            break;
        endif
        seen(s(j)) = true;
        maxLen = max(maxLen, j - i + 1);
    endfor
endfor

disp(maxLen);
