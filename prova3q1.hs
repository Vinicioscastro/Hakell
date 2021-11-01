lista1 = [1,5,6,3,4,9,3,1,2,5,1]
lista2 = "Haskell is awesome"

-- Vinicios Castro questão 01

removeIgual []     = []
removeIgual [a]    = [a]
removeIgual (x:xs) = x:(removeIgual ( filter (/=x) xs))


freq [] = []
freq lista1 = removeIgual( [(lista1!!i, length (filter (==lista1!! i) lista1)  )| i<-[0.. (length lista1 -1)]] )
 
-- freq "Haskell is awesome"
-- ou
-- freq [1,5,6,3,4,9,3,1,2,5,1]
-- ou
-- freq lista1
-- ou 
-- freq lista2

