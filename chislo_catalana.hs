fact 0 = 1
fact 1 = 1
fact n | n < 0 = error "fact(n), => n >= 0"
       | otherwise = n * fact(n-1)
cat n | (fact(n) * fact(n+1)) == 0 = error "(fact(n) * fact(n+1)) != 0"
      | otherwise = fact(2*n)/(fact(n) * fact(n+1))