--Pedro Grimaldi Hansel
--Matrícula 202165117AC

import System.Random (randomRIO)
import Data.List (delete)

-- Definindo o tipo Dado
data Dado = Dado { face :: Int } deriving (Show, Eq)

-- Obtendo as faces adjacentes possíveis de um dado
facesAdjacentes :: Int -> [Int]
facesAdjacentes f
    | f == 1 = [2, 3, 4, 5] -- Faces adjacentes a 1
    | f == 2 = [1, 3, 4, 6] -- Faces adjacentes a 2
    | f == 3 = [1, 2, 5, 6] -- Faces adjacentes a 3
    | f == 4 = [1, 2, 5, 6] -- Faces adjacentes a 4
    | f == 5 = [1, 3, 4, 6] -- Faces adjacentes a 5
    | f == 6 = [2, 3, 4, 5] -- Faces adjacentes a 6
    | otherwise = [] -- Caso inválido

-- Função para criar dados com faces aleatórias
criarDados :: Int -> IO [Dado]
criarDados n = sequence $ replicate n (Dado <$> randomRIO (1, 6))

-- Função para imprimir o estado dos dados
imprimirEstado :: [Dado] -> IO ()
imprimirEstado dados = do
    putStrLn "Estado atual dos dados:"
    mapM_ (\(i, d) -> putStrLn $ show i ++ ": " ++ show (face d)) (zip [1..] dados)
    putStrLn "----------------------"

-- Função para rotacionar um dado para uma nova face
rotacionarDadoNaPosicao :: [Dado] -> Int -> Int -> [Dado]
rotacionarDadoNaPosicao dados index novaFace =
    take index dados ++ [Dado novaFace] ++ drop (index + 1) dados

-- Função para remover um dado da lista
removeDado :: [Dado] -> Int -> [Dado]
removeDado dados index = delete (dados !! index) dados

-- Obtendo as faces adjacentes válidas e menores
facesAdjacentesValidas :: Int -> [Int]
facesAdjacentesValidas f = filter (< f) (facesAdjacentes f)

-- Função para determinar se uma configuração é perdedora
configuracaoPerdedora :: [Dado] -> Bool
configuracaoPerdedora dados
    | all (\d -> face d == 2 || face d == 5) dados = True
    | length dados == 1 = face (head dados) `elem` [2, 5]
    | length dados == 2 = let [d1, d2] = dados
                           in (face d1 == face d2) ||
                              (face d1 + face d2 == 7)
    | otherwise = let dadosValidos = filter (\d -> face d /= 2 && face d /= 5) dados
                      paresValidos = not (any (\(d1, d2) -> configuracaoPerdedora [d1, d2]) (combinarPares dadosValidos))
                  in not paresValidos

-- Função para verificar se a configuração é vencedora
configuracaoVencedora :: [Dado] -> Bool
configuracaoVencedora = not . configuracaoPerdedora

-- Função para combinar pares de dados
combinarPares :: [Dado] -> [(Dado, Dado)]
combinarPares [] = []
combinarPares (x:xs) = [(x, y) | y <- xs] ++ combinarPares xs

-- Função para representar a jogada do jogador
jogadaJogador :: [Dado] -> IO [Dado]
jogadaJogador dados = do
    imprimirEstado dados
    putStrLn "Escolha um dado para rotacionar ou remover (digite o número do dado):"
    escolha <- getLine
    let index = read escolha - 1
    if index < 0 || index >= length dados
        then do
            putStrLn "Escolha inválida! Tente novamente."
            jogadaJogador dados
        else do
            let dadoEscolhido = dados !! index
            if face dadoEscolhido == 1
                then do
                    putStrLn $ "Você removeu o dado com face " ++ show (face dadoEscolhido)
                    return (removeDado dados index)
                else do
                    let facesValidas = facesAdjacentesValidas (face dadoEscolhido)
                    putStrLn $ "Faces possíveis após rotação: " ++ show facesValidas
                    putStrLn "Escolha uma nova face para o dado:"
                    novaFace <- getLine
                    let novaFaceInt = read novaFace
                    if novaFaceInt `elem` facesValidas
                        then do
                            putStrLn $ "Você rotacionou o dado de face " ++ show (face dadoEscolhido) ++ " para " ++ show novaFaceInt
                            return (rotacionarDadoNaPosicao dados index novaFaceInt)
                        else do
                            putStrLn "Escolha inválida! A nova face deve ser menor que a face atual e estar entre as faces possíveis."
                            jogadaJogador dados

-- Função para jogar no nível fácil
jogadaComputadorFacil :: [Dado] -> IO [Dado]
jogadaComputadorFacil dados = do
    index <- randomRIO (0, length dados - 1)
    let dadoEscolhido = dados !! index
    if face dadoEscolhido == 1
        then do
            putStrLn $ "Computador removeu o dado com face " ++ show (face dadoEscolhido)
            return (removeDado dados index)
        else do
            let facesPossiveis = facesAdjacentesValidas (face dadoEscolhido)
            if null facesPossiveis
                then do
                    putStrLn $ "Computador não pode rotacionar o dado com face " ++ show (face dadoEscolhido)
                    return dados
                else do
                    novaFaceIndex <- randomRIO (0, length facesPossiveis - 1)
                    let faceEscolhida = facesPossiveis !! novaFaceIndex
                    putStrLn $ "Computador rotacionou o dado de face " ++ show (face dadoEscolhido) ++ " para " ++ show faceEscolhida
                    return (rotacionarDadoNaPosicao dados index faceEscolhida)

-- Função para jogar no nível difícil
jogadaComputadorDificil :: [Dado] -> IO [Dado]
jogadaComputadorDificil dados = do
    let jogadasPossiveis = [(index, rotacionarDadoNaPosicao dados index novaFace) |
                            index <- [0..length dados - 1],
                            novaFace <- facesAdjacentesValidas (face (dados !! index))]
    let jogadasRemocao = [(index, removeDado dados index) |
                          index <- [0..length dados - 1],
                          face (dados !! index) == 1]
    
    let jogadasVencedoras = filter (configuracaoPerdedora . snd) jogadasRemocao ++
                            filter (configuracaoPerdedora . snd) jogadasPossiveis
    if null jogadasVencedoras
        then jogadaComputadorFacil dados
        else do
            let (index, novaConfiguracao) = head jogadasVencedoras
            let dadoEscolhido = dados !! index
            let facesPossiveis = facesAdjacentesValidas (face dadoEscolhido)
            if null facesPossiveis
                then return novaConfiguracao
                else do
                    let novaFace = head facesPossiveis -- Correção para escolher uma face válida
                    putStrLn $ "Computador rotacionou o dado de face " ++ show (face dadoEscolhido) ++ " para " ++ show novaFace
                    return novaConfiguracao

-- Função principal do jogo
jogar :: [Dado] -> Bool -> Bool -> IO ()
jogar dados jogador humano = do
    if null dados
        then if not jogador
            then putStrLn "Você venceu!"
            else putStrLn "O computador venceu!"
    else do
        putStrLn "----------------------"
        if jogador
            then do
                putStrLn "Sua vez de jogar!"
                novosDados <- jogadaJogador dados
                jogar novosDados False humano
            else do
                putStrLn "Vez do computador!"
                novosDados <- if humano
                    then jogadaComputadorFacil dados
                    else jogadaComputadorDificil dados
                jogar novosDados True humano

-- Função principal para iniciar o jogo
main :: IO ()
main = do
    putStrLn "Escolha o nível de dificuldade: (1) Fácil ou (2) Difícil"
    nivel <- getLine
    let dificuldade = read nivel :: Int
    if dificuldade /= 1 && dificuldade /= 2
        then do
            putStrLn "Escolha inválida! Tente novamente."
            main
        else do
            putStrLn "Quantos dados deseja jogar?"
            quantidade <- getLine
            let numDados = read quantidade :: Int
            dados <- criarDados numDados
            putStrLn "Configuração inicial dos dados:"
            imprimirEstado dados
            putStrLn "Você começará a jogar contra o computador."
            if dificuldade == 2
                then do
                    putStrLn "Vez do computador!"
                    novosDados <- jogadaComputadorDificil dados
                    jogar novosDados True (dificuldade == 1)
                else jogar dados True (dificuldade == 1)