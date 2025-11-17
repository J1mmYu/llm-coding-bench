line = input()
result = ''.join(char for char in line if char.lower() not in 'aeiou')
print(result)
