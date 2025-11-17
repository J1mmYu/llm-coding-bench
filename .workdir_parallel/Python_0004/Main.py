line = input().strip()
stack = []
pairs = {'(': ')', '[': ']', '{': '}'}
for char in line:
    if char in pairs:
        stack.append(char)
    elif char in pairs.values():
        if not stack or pairs[stack.pop()] != char:
            print("NO")
            exit()
if stack:
    print("NO")
else:
    print("YES")
