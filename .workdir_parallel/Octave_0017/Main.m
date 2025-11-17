n = input('');
arr = input('');

max_len = 1;
current_len = 1;

for i = 2:n
    if arr(i) > arr(i-1)
        current_len = current_len + 1;
        max_len = max(max_len, current_len);
    else
        current_len = 1;
    endif
endfor

disp(max_len);
