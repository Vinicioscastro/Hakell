{- pot:: Int->Int->Int
pot x y
    |y == 1 = x
    |y == 0 = 0
    |y>1 = x*pot x (y-1) -}
    
lista = ["a", "b", "c", "d"]
lista2 = [1,5,6,3,4,9,3,1,2,5,1]

contavizinhos :: [Char] -> Int
contavizinhos [] = 0
contavizinhos (a:x)
    |length (a:x) ==1 = 0
    |a == head x = 1 + contavizinhos x
    |otherwise = contavizinhos x
-- length (filter (==4) lista2)   

[(lista!!i, i)| i<-[0.. (length lista -1)]]

[(lista!!i, length (filter (==i) lista2)  )| i<-[0.. (length lista -1)]]

-- resposta
[(lista2!!i, length (filter (==lista2!! i) lista2)  )| i<-[0.. (length lista2 -1)]]