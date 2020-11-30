import sys
s = 0
for line in sys.stdin:
    key, value = line.split()
    if(key == '42'):
        s += int(value)
print(s)
