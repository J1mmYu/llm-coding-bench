line = input('', 's');
if isempty(line)
  return
endif

i = 1;
while i <= length(line)
  current = line(i);
  count = 1;
  while i + count <= length(line) && line(i + count) == current
    count++;
  endwhile
  printf('%c%d', current, count);
  i += count;
endwhile
printf('\n');
