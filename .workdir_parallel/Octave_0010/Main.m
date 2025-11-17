line = input('', 's');
words = strsplit(line);
words = words(~cellfun(@isempty, words));

if isempty(words)
    return;
endif

[unique_words, ~, idx] = unique(words);
counts = accumarray(idx, 1);

[sorted_counts, sort_idx] = sort(counts, 'descend');
sorted_words = unique_words(sort_idx);

for i = 1:length(sorted_words)
    if sorted_counts(i) == sorted_counts(end) || (i < length(sorted_words) && sorted_counts(i) ~= sorted_counts(i+1))
        start_idx = i;
        end_idx = i;
        while end_idx < length(sorted_words) && sorted_counts(end_idx+1) == sorted_counts(start_idx)
            end_idx++;
        endwhile
        if start_idx ~= end_idx
            same_count_words = sorted_words(start_idx:end_idx);
            same_count_words = sort(same_count_words);
            for j = 1:length(same_count_words)
                printf('%s %d\n', same_count_words{j}, sorted_counts(start_idx));
            endfor
            i = end_idx;
        else
            printf('%s %d\n', sorted_words{i}, sorted_counts(i));
        endif
    elseif i == length(sorted_words)
        printf('%s %d\n', sorted_words{i}, sorted_counts(i));
    endif
endfor
```

Wait, let me simplify this:

```octave
line = input('', 's');
words = strsplit(line);
words = words(~cellfun(@isempty, words));

if isempty(words)
    return;
endif

[unique_words, ~, idx] = unique(words);
counts = accumarray(idx, 1);

result = [num2cell(counts), unique_words'];
result = sortrows(result, [1, 2], {'descend', 'ascend'});

for i = 1:size(result, 1)
    printf('%s %d\n', result{i, 2}, result{i, 1});
endfor
