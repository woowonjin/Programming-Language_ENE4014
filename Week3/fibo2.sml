fun fibo 0 = (1, 1)
  | fibo n = 
        let val (n1, n2) = fibo(n-1)
        in
          (n1+n2, n1)
        end              
