a = input('', 's');
b = input('', 's');

% Remove leading/trailing whitespace
a = strtrim(a);
b = strtrim(b);

% Handle negative numbers
sign_a = 1;
sign_b = 1;
if a(1) == '-'
    sign_a = -1;
    a = a(2:end);
endif
if b(1) == '-'
    sign_b = -1;
    b = b(2:end);
endif

% Convert strings to digit arrays (reverse order for easier addition)
digits_a = fliplr(a - '0');
digits_b = fliplr(b - '0');

% If signs are different, convert to subtraction
if sign_a ~= sign_b
    % Compare absolute values
    len_a = length(digits_a);
    len_b = length(digits_b);
    
    if len_a > len_b || (len_a == len_b && strcmp(a, b) > 0)
        % |a| > |b|
        result_sign = sign_a;
        [longer, shorter] = deal(digits_a, digits_b);
    elseif len_a < len_b || (len_a == len_b && strcmp(a, b) < 0)
        % |a| < |b|
        result_sign = sign_b;
        [longer, shorter] = deal(digits_b, digits_a);
    else
        % |a| == |b|
        disp('0');
        return;
    endif
    
    % Subtraction
    max_len = length(longer);
    shorter = [shorter, zeros(1, max_len - length(shorter))];
    result = zeros(1, max_len);
    borrow = 0;
    
    for i = 1:max_len
        diff = longer(i) - shorter(i) - borrow;
        if diff < 0
            diff = diff + 10;
            borrow = 1;
        else
            borrow = 0;
        endif
        result(i) = diff;
    endfor
else
    % Same sign - addition
    result_sign = sign_a;
    max_len = max(length(digits_a), length(digits_b));
    digits_a = [digits_a, zeros(1, max_len - length(digits_a))];
    digits_b = [digits_b, zeros(1, max_len - length(digits_b))];
    
    result = zeros(1, max_len + 1);
    carry = 0;
    
    for i = 1:max_len
        sum_val = digits_a(i) + digits_b(i) + carry;
        result(i) = mod(sum_val, 10);
        carry = floor(sum_val / 10);
    endfor
    result(max_len + 1) = carry;
endif

% Remove leading zeros
while length(result) > 1 && result(end) == 0
    result = result(1:end-1);
endwhile

% Convert back to string
result_str = char(fliplr(result) + '0');

% Add sign if negative
if result_sign < 0
    result_str = ['-', result_str];
endif

disp(result_str);
