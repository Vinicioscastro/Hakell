
--Vinicios Pereira Da Costa Castro

import System.IO
import System.IO.Error

type Dataa = (Int, Int, Int)

type Pessoa = (Int, String, String, Float, Dataa)

mostrarTrabalhador :: Pessoa -> IO ()

mostrarTrabalhador (id,nome, cargo,salario,(dia,mes,ano)) = do

  putStrLn ("\nID: " ++ (show id))

  putStrLn ("Nome: " ++ nome)

  putStrLn ("Cargo: " ++ cargo)

  putStrLn ("Salario: " ++ (show salario))

  putStrLn ("Data de Entrada: " ++ (show dia) ++ "/" ++ (show mes) ++ "/" ++ (show ano))

mostrarSomenteNome :: Pessoa -> IO ()

mostrarSomenteNome (id,nome, cargo,salario,(dia,mes,ano)) = do
  putStrLn ("\nNome: " ++ nome)


criarNovoFuncionario :: IO (Pessoa)
criarNovoFuncionario = do
  putStrLn "Digite o ID do Profissional:"
  id1 <- getLine
  putStrLn "Digite o Nome do Profissional:"
  nome1 <- getLine
  putStrLn "Digite o Cargo do Profissional:"
  cargo1 <- getLine
  putStrLn "Digite o Salario do Profissional:"
  salario1 <- getLine
  putStrLn "Digite a Data de Entrada do Profissional na empresa:"
  putStrLn "Dia:"
  dia1 <- getLine
  putStrLn "Mes:"
  mes1 <- getLine
  putStrLn "Ano:"
  ano1 <- getLine
  let id = (read id1 :: Int)
  let salario = (read salario1 :: Float)
  let dia = (read dia1 :: Int)
  let mes = (read mes1 :: Int)
  let ano = (read ano1 :: Int)
  let novoFuncionario = (id,nome1,cargo1,salario,(dia,mes,ano))
  return novoFuncionario

adiconarAlista :: [Pessoa] -> Pessoa -> [Pessoa]
adiconarAlista list novoFuncionario = [novoFuncionario] ++ list


ganhaMais :: [Pessoa] -> Float -> [Pessoa]
ganhaMais list valor = filter (\(_,_,_,salario,_) -> salario > valor) list


funcionarioCargo :: [Pessoa] -> String -> [Pessoa]
funcionarioCargo list cargoo = filter (\(_,_,cargo,_,_) -> cargo == cargoo) list


menu :: [Pessoa] -> IO ()
menu list = do
  putStrLn "\n--------||   FUNÇÕES  ||--------"
  putStrLn "--1-- = Imprimir Funcionarios"
  putStrLn "--2-- = Criar Funcionario"
  putStrLn "--3-- = Sálario mais que"
  putStrLn "--4-- = Cargo"
  putStrLn "--5-- = Funcionario mais antigo"
    
  opt <- getChar
  getChar
  case opt of
    '1' -> do
      mapM_ mostrarTrabalhador list
      menu list
    '2' -> do
      novoFuncionario <- criarNovoFuncionario
      let db = adiconarAlista list novoFuncionario
      menu db
    
    '3' -> do
      putStrLn "Digite o Valor:"
      valor1 <- getLine
      let valor = (read valor1 :: Float)
      mapM_ mostrarSomenteNome (ganhaMais list valor)
      menu list
    '4' -> do
      putStrLn "Digite o Cargo:"
      cargoo <- getLine
      mapM_ mostrarTrabalhador (funcionarioCargo list cargoo)
      menu list
    --'5' -> do
      --
     -- menu list
    
    _ -> do
      putStrLn "\nTente Novamente!"
      menu list

main :: IO ()
main = do
  menu []
  return ()