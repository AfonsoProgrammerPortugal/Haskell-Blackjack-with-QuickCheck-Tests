module Blackjack (
    Baralho,
    valores,
    naipes,
    baralhoNormal,
    tamanho,
    EstadoJogo(status, baralho, creditosRonda, aposta, maoCasa,
                 creditos, maoJogador),
    inicializa,
    terminado,
    distribuirCartas,
    charInt,
    valorCarta,
    valorMao,
    menorValorMao,
    maoEvinteUm,
    hitJogador,
    simulaJogadaCasa,
    hitCasa,
    jogadorRebentou,
    casaRebentou,
    rebentou,
    vitoriaJogador,
    derrotaJogador,
    empateJogador,
    maiorValorMenorIgualQue21,
    shuffleBaralho,
    apostaValida,
    listaParaString,
    apostar,
    sair,
    resetar,
    emTeste,
    compararValores',
) where

import System.Random (StdGen, Random (randomR))
import Test.QuickCheck

type Baralho = [String]

valores :: [Char]
valores = ['A'] ++ ['2'..'9'] ++ ['T', 'J', 'Q', 'K']

naipes :: [Char]
naipes = ['S', 'H', 'D', 'C']

baralhoNormal :: Baralho
baralhoNormal = [ [v, n] | n <- naipes, v <- valores]

tamanho :: Baralho -> Int
tamanho = length

data EstadoJogo = EstadoJogo {
    baralho :: Baralho,
    maoJogador :: [String],
    maoCasa :: [String],
    creditos :: Int,
    creditosRonda :: Int,
    aposta :: Int,
    status :: String,
    emTeste :: Bool
} deriving (Eq)

inicializa :: Baralho -> Int -> EstadoJogo
inicializa x creditos = EstadoJogo {
    baralho = x,
    maoJogador = [],
    maoCasa = [],
    creditos = creditos,
    creditosRonda = creditos,
    aposta = 0,
    status = "em jogo",
    emTeste = False
}

terminado :: EstadoJogo -> Bool
terminado x = semCreditos x || menosDeVinteCartas x

semCreditos :: EstadoJogo -> Bool
semCreditos x = creditos x <= 0

menosDeVinteCartas :: EstadoJogo -> Bool
menosDeVinteCartas x = tamanho (baralho x) <= 20

distribuirCartas :: EstadoJogo -> EstadoJogo
distribuirCartas x = x {
    baralho = drop 4 (baralho x),
    maoJogador = take 2 (baralho x),
    maoCasa = take 2 (drop 2 (baralho x))
}

charInt :: Char -> Int
charInt x = read [x] :: Int

valorCarta :: String -> [Int]
valorCarta (x:_)
    | x == 'A' = [1, 11]
    | x `elem` ['T', 'J', 'Q', 'K'] = [10]
    | otherwise = [charInt x]

valorMao :: Baralho -> [Int]
valorMao [] = [0]
valorMao (x:xs) = [x+y | x <- valorCarta x, y <- valorMao xs]

menorValorMao :: Baralho -> Int
menorValorMao x = minimum (valorMao x)

maoEvinteUm :: [String] -> Bool
maoEvinteUm x = 21 `elem` (valorMao x)

hitJogador :: EstadoJogo -> EstadoJogo
hitJogador x = x {
    baralho = tail (baralho x),
    maoJogador = maoJogador x ++ [head . baralho $ x]
}

hitCasa :: EstadoJogo -> EstadoJogo
hitCasa x = x {
    baralho = tail (baralho x),
    maoCasa = maoCasa x ++ [head . baralho $ x]
}

jogadorRebentou :: EstadoJogo -> Bool
jogadorRebentou x = rebentou (maoJogador x)

casaRebentou :: EstadoJogo -> Bool
casaRebentou x = rebentou (maoCasa x)

rebentou :: [String] -> Bool
rebentou x = menorValorMao x > 21

vitoriaJogador :: EstadoJogo -> EstadoJogo
vitoriaJogador x = x {
    creditos = creditos x + (aposta x) * 2,
    status = "vitoria"
}

derrotaJogador :: EstadoJogo -> EstadoJogo
derrotaJogador x = x {
    creditos = creditos x,
    status = "derrota"
}

empateJogador :: EstadoJogo -> EstadoJogo 
empateJogador x = x {
    creditos = creditos x + aposta x,
    status = "empate"
}

maiorValorMenorIgualQue21 :: [String] -> Int
maiorValorMenorIgualQue21 xs = maximum (filter (<= 21) (valorMao xs))

shuffleBaralho :: [a] -> StdGen -> [a]
shuffleBaralho xs gen = shuffle' xs (length xs) gen
  where
    shuffle' :: [a] -> Int -> StdGen -> [a]
    shuffle' [] _ _ = []
    shuffle' xs n gen = (xs !! i) : shuffle' (take i xs ++ drop (i + 1) xs) (n - 1) gen'
      where
        (i, gen') = randomR (0, n-1) gen

apostaValida :: Int -> EstadoJogo -> Bool
apostaValida x j = x >= 1 && x <= creditos j

listaParaString :: [String] -> String
listaParaString [] = ""
listaParaString [x] = x
listaParaString (x:xs) = x ++ " " ++ listaParaString xs

apostar :: Int -> EstadoJogo -> EstadoJogo
apostar x y = y {
    creditosRonda = creditos y,
    creditos = creditos y - x,
    aposta = x
}

sair :: EstadoJogo -> EstadoJogo
sair x = x {
    status = "saiu"
}

resetar :: EstadoJogo -> EstadoJogo
resetar x = x {
    maoJogador = [],
    maoCasa = [],
    aposta = 0
}

compararValores' :: EstadoJogo -> EstadoJogo
compararValores' x
    | menorValorMao (maoJogador x) > 21 = derrotaJogador x
    | menorValorMao (maoCasa x) > 21 = vitoriaJogador x
    | maiorValorMenorIgualQue21 (maoJogador x) > maiorValorMenorIgualQue21 (maoCasa x) = vitoriaJogador x
    | maiorValorMenorIgualQue21 (maoJogador x) < maiorValorMenorIgualQue21 (maoCasa x) = derrotaJogador x
    | otherwise = empateJogador x

simulaJogadaCasa :: EstadoJogo -> EstadoJogo
simulaJogadaCasa x
    | casaRebentou x = x
    | maoEvinteUm (maoCasa x) = x
    | maiorValorMenorIgualQue21 (maoCasa x) >= 17 = x
    | menorValorMao (maoCasa x) < 17 = simulaJogadaCasa (hitCasa x)
    | otherwise = x