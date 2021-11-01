-- VINICIOS PEREIRA DA COSTA CASTRO -> PROVA -> 10

salario :: Int -> Float
salario 1 = 772.25
salario 2 = 2375.0
salario 3 = 1778.5
salario 4 = 6520.0
salario 5 = 3447.35
salario 6 = 5225.75
salario 7 = 8932.0
salario 8 = 648.5
salario 9 = 1982.4
salario 10 = 2557.45
salario _ = 0

------- Gastos com Sálario ------------ Letra A
gasto :: Int -> Float
gasto n
      | n==0 = salario 0
      | n > 0 = salario n + gasto ( n - 1)
      | otherwise = 0


---------- Recebe mais que X ------------ Letra B
recebeAcima:: Float-> Int -> Int
recebeAcima valor n
    | n == 0 = 0
    | salario n > valor = 1 + recebeAcima valor (n-1)
    | salario n < valor = recebeAcima valor (n-1)
    | otherwise = 0

---------- Menor Diferença ------------ Letra C

dizersalario:: Float -> Float -> Int -> Float
dizersalario valor diferenca n
    |valor + diferenca == salario n = salario n
    |valor - diferenca == salario n = salario n
    |otherwise = dizersalario (valor) (diferenca) (n-1)

maispro :: Float->Int ->Float
maispro valor n
    |n == 0 = valor
    |n > 0 = min (abs (salario n - valor)) (maispro (valor) (n-1))
    |otherwise = 0

--------- Contribuições ------------- Letra D

calcularcont :: Int -> Float
calcularcont n
  |n == 0 = 0
  |salario n >= 6433.57 = (salario n * 0.22) + calcularcont ( n-1 )
  |salario n >= 3305.23 = (salario n * 0.14) + calcularcont ( n-1 )
  |salario n >= 2203.49 = (salario n * 0.12) + calcularcont ( n-1 )
  |salario n >= 1100.01 = (salario n * 0.09) + calcularcont ( n-1 )
  |salario n < 1100.01 = 0 + calcularcont(n-1)
  |otherwise = 0

--main = print $ gasto 10

--main = print $ recebeAcima 2500 10

--main = print $ dizersalario 5000 (maispro 5000 10) 10

main = print $ calcularcont 10