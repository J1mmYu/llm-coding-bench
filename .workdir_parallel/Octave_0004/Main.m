line = input('', 's');
stack = {};
balanced = true;
pairs = containers.Map({')', ']', '}'}, {'(', '[', '{'});

for i = 1:length(line)
    c = line(i);
    if c == '(' || c == '[' || c == '{'
        stack{end+1} = c;
    elseif c == ')' || c == ']' || c == '}'
        if isempty(stack) || ~strcmp(stack{end}, pairs(c))
            balanced = false;
            break;
        end
        stack(end) = [];
    end
end

if balanced && isempty(stack)
    printf('YES\n');
else
    printf('NO\n');
end
