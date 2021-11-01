
--Vinicios Pereira Da Costa Castro

import System.IO
import System.IO.Error


type Produto = (String, Int, Float)

mostrarProduto :: Produto -> IO ()

mostrarProduto (nome, quantidade, valor) = do

  putStrLn ("\nNome: " ++ nome)

  putStrLn ("Quantidade: " ++ (show quantidade))

  putStrLn ("Valor: " ++ (show valor))


{- mostrarSomenteNome :: Produto -> IO ()

mostrarSomenteNome (nome, quantidade,valor) = do
  putStrLn ("\nNome: " ++ nome) -}


criarProduto :: IO (Produto)
criarProduto = do
 
  putStrLn "Nome do Produto:"
  nome1 <- getLine
  putStrLn "Quantidade:"
  quantidade1 <- getLine
  putStrLn "Valor:"
  valor1 <- getLine

  let quantidade = (read quantidade1 :: Int)
  let valor = (read valor1 :: Float)
  
  let novoPro = (nome1,quantidade,valor)
  return novoPro

adiconarAlista :: [Produto] -> Produto -> [Produto]
adiconarAlista list novoProduto = [novoProduto] ++ list

buscarProduto :: [Produto] -> String -> [Produto]
buscarProduto lista nomeProduto = filter(\(nome,_,_) -> nome == nomeProduto) lista

atualizar :: [Produto] -> Int -> [Produto]
atualizar lista novovalor = filter ((\(_,quant,_) -> quant > 0 )) li(_,novovalor,_)

deletar:: [Produto] -> [Produto]
deletar list = filter (\(_,quantidade,_) -> quantidade > 0) list


{- 
valor :: [Produto] -> Float
valor \(nome, q, v) = q * v -}

menu :: [Produto] -> IO ()
menu list = do
  putStrLn "\n--------||   FUNÇÕES  ||--------"
  putStrLn "--1-- = Imprimir Produtos"
  putStrLn "--2-- = Criar Produto"
  putStrLn "--3-- = Somar Valores"
  putStrLn "--4-- = Vender"
  putStrLn "--5-- = Buscar por nome"
    
  opt <- getChar
  getChar
  case opt of
    '1' -> do
      mapM_ mostrarProduto list
      menu list
    '2' -> do
      novoFuncionario <- criarProduto
      let db = deletar (adiconarAlista list novoFuncionario)
      
      menu db
    
    {- '3' -> do
      putStrLn "Digite o Valor:"
      valor1 <- getLine
      let valor = (read valor1 :: Float)
      mapM_ mostrarSomenteNome (ganhaMais list valor)
      menu list
    '4' -> do
      putStrLn "Digite o Cargo:"
      cargoo <- getLine
      mapM_ mostrarProduto (funcionarioCargo list cargoo)
      menu list  -}

    '5' -> do
      putStrLn "Digite o Produto:"
      produto <- getLine
      mapM_ mostrarProduto (buscarProduto list produto)
      menu list

    '6' -> do
      putStrLn "Digite o Produto:"
      produto <- getLine
      mapM_ atulizar (buscarProduto list produto)
      menu list
    

    _ -> do
      putStrLn "\nTente Novamente!"
      menu list

main :: IO ()
main = do
  menu []
  return ()