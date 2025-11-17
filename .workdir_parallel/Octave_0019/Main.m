line = input('', 's');
cleaned = lower(regexprep(line, '[^a-zA-Z0-9]', ''));
if strcmp(cleaned, fliplr(cleaned))
    disp('YES');
else
    disp('NO');
endif
