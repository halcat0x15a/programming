def fib(n):
    buf = [0, 1]
    for i in range(2, n):
        buf.append(buf[i - 1] + buf[i - 2])
    return buf

for n in fib(100):
    print(n)
