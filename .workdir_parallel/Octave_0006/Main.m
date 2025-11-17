line = fgets(stdin);
if line == -1
    return;
endif

line = strtrim(line);
fields = {};
i = 1;
n = length(line);

while i <= n
    if line(i) == '"'
        % Quoted field
        field = '';
        i = i + 1; % Skip opening quote
        while i <= n
            if line(i) == '"'
                if i < n && line(i+1) == '"'
                    % Escaped quote
                    field = [field '"'];
                    i = i + 2;
                else
                    % End of quoted field
                    i = i + 1;
                    break;
                endif
            else
                field = [field line(i)];
                i = i + 1;
            endif
        endwhile
        fields{end+1} = field;
        % Skip comma if present
        if i <= n && line(i) == ','
            i = i + 1;
        endif
    else
        % Unquoted field
        field = '';
        while i <= n && line(i) != ','
            field = [field line(i)];
            i = i + 1;
        endwhile
        fields{end+1} = field;
        % Skip comma
        if i <= n && line(i) == ','
            i = i + 1;
        endif
    endif
endwhile

printf('%d\n', length(fields));
