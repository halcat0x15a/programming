def fib(n)
  buf = [0, 1]
  for i in 2...n
    buf[i] = buf[i - 1] + buf[i - 2]
  end
  buf
end

for n in fib(100)
  puts n
end
