import sys

lines = 0
words = 0
bytes_count = 0

for line in sys.stdin.buffer:
    lines += 1
    bytes_count += len(line)
    words += len(line.decode().split())

print(f"{lines} {words} {bytes_count}")
