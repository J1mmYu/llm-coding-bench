line = input()
if not line:
    print()
else:
    result = []
    i = 0
    while i < len(line):
        char = line[i]
        count = 1
        while i + count < len(line) and line[i + count] == char:
            count += 1
        result.append(f"{char}{count}")
        i += count
    print("".join(result))
