import sys

max_line = ""
max_distinct = -1

for line in sys.stdin:
    line = line.rstrip('\n')
    distinct = len(set(line))
    if distinct > max_distinct:
        max_distinct = distinct
        max_line = line

print(max_line)
