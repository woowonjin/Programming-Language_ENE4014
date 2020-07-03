fun fibo 0 = 1
  | fibo 1 = 1
  | fibo n = fibo(n-2) + fibo(n-1)
