import re

s = input().strip()

def is_valid_ipv4(ip):
    parts = ip.split('.')
    if len(parts) != 4:
        return False
    
    for part in parts:
        if not part:
            return False
        if not part.isdigit():
            return False
        if len(part) > 1 and part[0] == '0':
            return False
        num = int(part)
        if num < 0 or num > 255:
            return False
    
    return True

print(1 if is_valid_ipv4(s) else 0)
