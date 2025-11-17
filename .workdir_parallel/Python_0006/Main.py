import sys
import csv

line = sys.stdin.readline().strip()
reader = csv.reader([line])
fields = next(reader)
print(len(fields))
