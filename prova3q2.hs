
-- Vinicios Castro questão 02

l1 = ["sol", "bola", "casa"]
l2 = ["mola", "mala", "cama", "livro"]

diferenca l1 l2
  |null l1 || null l2 = max (length l1) (length l2)
  |(head l1)==(head l2) = diferenca (tail l1) (tail l2)
  |otherwise = 1+diferenca (tail l1) (tail l2)

hammingAux str [x] (y,z,n)
  |(diferenca x str) < n = (str,x,(diferenca x str))
  |otherwise = (y,z,n)

hammingAux str (cabeca:rabo) (y,z,n)
  |(diferenca cabeca str) < n = hammingAux str rabo (str,cabeca,(diferenca cabeca str))
  |otherwise = hammingAux str rabo (y,z,n)

hammingDistancia lista1 lista2 = [hammingAux (lista1!!i) lista2 (lista1!!i,lista2!!1,(diferenca (lista1!!i) (lista2!!1)))| i<-[0.. (length lista1 -1)]]


-- para rodar o código
-- hammingDistancia l1 l2