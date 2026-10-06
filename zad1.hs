-- What are the values of the following expressions?

main = do
  print(10) -- 10
  print(5 + 3 + 4) -- 12
  print(9 - 1) -- 8
  print(6 / 2) -- 3.0
  print(2 * 3 + 4 * 6) -- 30
  print(7 < 9) -- True
  print(3 <= 5 && 5 <= 3) -- False
  print(let a = 3 in a + 1) -- 4
  print(let a = 1 in let b = a + 1 in a + b + a * b) -- 5 (a=1, b=2)
  --print(a) -- variable not in scope
  print(let square n = n * n in square 4) -- 16
  print(let fac n = if n == 0 then 1 else n * fac (n-1) in fac 5) -- 120 (fac(5))

-- uruchamianie -> runghc zad1.hs