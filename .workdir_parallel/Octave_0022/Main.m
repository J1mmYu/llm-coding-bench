line = input('', 's');
line = strrep(line, ',', ' ');
nums = str2num(line);
disp(sum(nums));
