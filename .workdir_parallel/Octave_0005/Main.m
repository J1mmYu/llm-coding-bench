n = input('');
arr = input('');

dp = ones(1, n);

for i = 2:n
    for j = 1:i-1
        if arr(j) < arr(i)
            dp(i) = max(dp(i), dp(j) + 1);
        endif
    endfor
endfor

printf('%d\n', max(dp));
