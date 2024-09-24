{-
Princípios de Programação
Projeto 3 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
p3_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

> stack ghc Main.hs

p3_59808_59886.zip

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes quatro tipos de instruções:

> ./Main ficheiro -- carrega um baralho para jogar Blackjack
> ./Main          -- carrega o baralho default.bar
> ./Main -n X     -- carrega um baralho aleatório formado por X baralhos normais de cartas
> ./Main -t       -- corre os testes
-}

import System.Environment ( getArgs )
import Blackjack
import System.Random ( getStdGen, Random(randomR), StdGen )
import Control.Monad (when, unless)
import System.Directory (doesFileExist)
import Test.QuickCheck
import Testes

-- A
main = do
    args <- getArgs
    case args of
        [] -> do
            -- A1
            baralho <- carregarBaralho "default.bar"
            comecarJogo baralho

        ["-n", n] -> do
            -- A3
            baralho <- carregarBaralhoAleatorio (read n :: Int)
            comecarJogo baralho
        ["-t"] -> do
            quickCheck prop_maoInicial
            quickCheck prop_creditos
            quickCheck prop_maoCasa
            quickCheck prop_sair
            quickCheck prop_hitJogador
            quickCheck prop_hitCasa
        [ficheiro] -> do
            -- A2
            baralho <- carregarBaralho ficheiro
            comecarJogo baralho
        _ -> do
            putStrLn "Argumentos inválidos."
            putStrLn "Utilização:"
            putStrLn " ./Main [ficheiro] (carrega um baralho para jogar Blackjack)"
            putStrLn " ./Main            (carrega o baralho default.bar)"
            putStrLn " ./Main -n X       (carrega um baralho aleatório formado por X baralhos normais de cartas)"
            putStrLn " ./Main -t         (corre os testes)"

-- B

comecarJogo :: Baralho -> IO ()
comecarJogo b = do
    unless (null b) $ do
        let jogo = inicializa b 100
        putStrLn ("cartas: " ++ show (tamanho (baralho jogo)))
        putStrLn ("creditos: " ++ show (creditos jogo))

        simularRonda jogo

carregarBaralho :: FilePath -> IO [String]
carregarBaralho x = do
    existe <- doesFileExist x
    if existe then do
        conteudo <- readFile x
        let baralho = lines conteudo
        return baralho
    else do
        putStrLn "Ficheiro não encontrado."
        return []

carregarBaralhoAleatorio :: Int -> IO [String]
carregarBaralhoAleatorio x = do
    let baralho = take (x * length baralhoNormal) (cycle baralhoNormal)

    gen <- getStdGen

    return (shuffleBaralho baralho gen)

-- B1

simularRonda :: EstadoJogo -> IO ()
simularRonda j = do
    if not (terminado j) then do
        j2 <- lerInputRonda j
        if status j2 == "saiu" then -- B2
            fimDoJogo j2
        else do
            let jogo = distribuirCartas j2

            printarMaos jogo
            vezDoJogador jogo -- B3
    else do
        fimDoJogo j

lerInputRonda :: EstadoJogo -> IO EstadoJogo
lerInputRonda j = do
    input <- getLine
    let comando = words input

    case comando of
        ["apostar", x] -> do
            let v = read x :: Int
            lerAposta v j

        -- B2
        ["sair"] -> do
            return (sair j)

        _ -> do
            putStrLn "Comando inválido."
            lerInputRonda j

lerAposta :: Int -> EstadoJogo -> IO EstadoJogo
lerAposta x j = do
    if apostaValida x j then do
        let jogo = apostar x j
        return jogo
    else do
        putStrLn ("Aposta inválida. Escolha um valor entre 1 e " ++ show (creditos j) ++ ".")
        lerInputRonda j

printarMaos :: EstadoJogo -> IO ()
printarMaos j = do
    putStrLn ("jogador: " ++ listaParaString (maoJogador j))
    putStrLn ("casa: " ++ listaParaString (maoCasa j))

vezDoJogador :: EstadoJogo -> IO ()
vezDoJogador j = do
    if jogadorRebentou j then do
        derrota j
    else if maoEvinteUm (maoJogador j) then do
        vezDaCasa j
    else do
        lerInputJogada j

-- B4, B5

lerInputJogada :: EstadoJogo -> IO ()
lerInputJogada j = do
    input <- getLine
    let comando = words input

    case comando of
        ["hit"] -> do
            let jogo = hitJogador j
            printarMaos jogo
            vezDoJogador jogo

        ["stand"] -> do
            vezDaCasa j

        _ -> do
            putStrLn "Comando inválido."
            lerInputJogada j

-- B6

vezDaCasa :: EstadoJogo -> IO ()
vezDaCasa j = do
    if casaRebentou j ||
        maoEvinteUm (maoCasa j) ||
        maiorValorMenorIgualQue21 (maoCasa j) >= 17
    then do
        printarMaos j
        compararValores j
    else do
        let jogo = hitCasa j
        vezDaCasa jogo

-- B7

compararValores :: EstadoJogo -> IO ()
compararValores j = do
    if menorValorMao (maoJogador j) > 21 then
        derrota j
    else if menorValorMao (maoCasa j) > 21 then
        vitoria j
    else if maiorValorMenorIgualQue21 (maoJogador j) > maiorValorMenorIgualQue21 (maoCasa j) then
        vitoria j
    else if maiorValorMenorIgualQue21 (maoJogador j) < maiorValorMenorIgualQue21 (maoCasa j) then
        derrota j
    else
        empate j

-- B8

fimDaRonda :: EstadoJogo -> IO ()
fimDaRonda j = do
    putStrLn ("cartas: " ++ show (tamanho (baralho j)))
    putStrLn ("creditos: " ++ show (creditos j))

    let jogo = resetar j

    simularRonda jogo

derrota :: EstadoJogo -> IO ()
derrota j = do
    putStrLn "Derrota"
    let jogo = derrotaJogador j
    fimDaRonda jogo

vitoria :: EstadoJogo -> IO ()
vitoria j = do
    putStrLn "Vitoria"
    let jogo = vitoriaJogador j
    fimDaRonda jogo

empate :: EstadoJogo -> IO ()
empate j = do
    putStrLn "Empate"
    let jogo = empateJogador j
    fimDaRonda jogo

-- B9

fimDoJogo :: EstadoJogo -> IO ()
fimDoJogo j = do
    putStrLn ("saldo final: " ++ show (creditos j))