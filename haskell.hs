--ghc 8.0.2

-- FATORIAL

fatorial:: Int->Int
fatorial n
    |n==0 =1
    |n>0 = n*fatorial(n-1)
    
    
-- FIBONACCI

fibonacci:: Int-> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Algarismos

contaNumero:: Int-> Int
contaNumero n
    |div n 10 == 0 = 1
    |otherwise = 1 + contaNumero (div n 10)

-- POTENCIA

pot:: Int->Int->Int
pot x y
    |y == 1 = x
    |y == 0 = 0
    |y>1 = x*pot x (y-1)
    
    


main = print $ contaNumero 153659874









