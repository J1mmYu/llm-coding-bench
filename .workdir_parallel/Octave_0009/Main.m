s = input('', 's');
parts = strsplit(s, '.');
valid = 0;
if length(parts) == 4
  valid = 1;
  for i = 1:4
    part = parts{i};
    if isempty(part)
      valid = 0;
      break;
    endif
    if length(part) > 1 && part(1) == '0'
      valid = 0;
      break;
    endif
    num = str2num(part);
    if isempty(num) || num != floor(num) || num < 0 || num > 255
      valid = 0;
      break;
    endif
    if num2str(num) != part
      valid = 0;
      break;
    endif
  endfor
endif
disp(valid);
