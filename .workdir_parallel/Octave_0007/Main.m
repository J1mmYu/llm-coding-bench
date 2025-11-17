s = input('', 's');
k = input('');

n = length(s);
min_substr = s(1:k);

for i = 2:(n-k+1)
    current = s(i:i+k-1);
    if strcmp(current, min_substr) < 0
        min_substr = current;
    endif
endfor

disp(min_substr);
