import re
line = input()
numbers = [int(x) for x in re.split(r'[,\s]+', line) if x]
print(sum(numbers))
