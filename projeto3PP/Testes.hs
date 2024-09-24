module Testes (prop_maoInicial, prop_creditos, prop_maoCasa, prop_sair, prop_hitJogador, prop_hitCasa) where
import Test.QuickCheck
import Blackjack
    ( apostar,
      baralhoNormal,
      compararValores',
      distribuirCartas,
      hitCasa,
      hitJogador,
      inicializa,
      listaParaString,
      maiorValorMenorIgualQue21,
      sair,
      tamanho,
      valorMao,
      EstadoJogo(status, baralho, creditosRonda, aposta, maoCasa,
                 creditos, maoJogador),
      simulaJogadaCasa )

-- C1 

-- propriedade: uma mão inicial (de duas cartas) vale sempre no máximo 21 pontos.

prop_maoInicial :: EstadoJogo -> Bool
prop_maoInicial j =
        let jogo = distribuirCartas j
        in tamanho (maoJogador jogo) == 2 && maiorValorMenorIgualQue21 (maoJogador jogo) <= 21

-- propriedade: o número de créditos do jogador após uma ronda é um de três valores possíveis n-a,n,n+a, onde n é o número de 
prop_creditos :: EstadoJogo -> Property
prop_creditos j =
        aposta j < creditos j ==>
        let jogo = jogar j
        in creditos jogo == creditosRonda jogo || creditos jogo == (creditosRonda jogo - aposta jogo) || creditos jogo == (creditosRonda jogo + aposta jogo)
        where jogar =
                compararValores' . hitCasa . hitJogador . distribuirCartas

-- propriedade: no final da vez da casa, a mão da casa tem sempre pelo menos 17 pontos.
prop_maoCasa :: EstadoJogo -> Property
prop_maoCasa j =
        aposta j < creditos j ==>
        let jogo = simulaJogadaCasa (hitJogador . distribuirCartas $ j)
        in maximum (valorMao (maoCasa jogo)) >= 17

-- propriedade: ao sair no início de uma ronda, o jogador não perde créditos
prop_sair :: EstadoJogo -> Bool
prop_sair j =
        let jogo = sair j
        in creditos jogo == creditos j

-- verificar se o tamanho da mão do jogador antes fazer hit é menor que a mão do jogador apos fazer hit outra vez
prop_hitJogador :: EstadoJogo -> Bool
prop_hitJogador j =
        let inicial = distribuirCartas j
            jogo = hitJogador inicial
        in tamanho (maoJogador jogo) > tamanho (maoJogador inicial)

-- propriedade: se a casa fizer hit, o número de cartas no baralho diminui em 1
prop_hitCasa :: EstadoJogo -> Bool
prop_hitCasa j = 
        let jogo = hitCasa j
        in tamanho (baralho jogo) == tamanho (baralho j) - 1

-- C2

newtype CartaValida = CV String
  deriving (Show, Eq)

instance Show EstadoJogo where
  show jogo = "EstadoJogo {baralho = " ++ show (baralho jogo) ++
              ", maoJogador = " ++ listaParaString (maoJogador jogo) ++
              ", maoCasa = " ++ listaParaString (maoCasa jogo) ++
              ", creditos = " ++ show (creditos jogo) ++
              ", creditosRonda = " ++ show (creditosRonda jogo) ++
              ", aposta = " ++ show (aposta jogo) ++
              ", status = " ++ show (status jogo) ++ "}"

newtype BaralhoValido = BV [String] deriving (Eq, Show)
instance Arbitrary BaralhoValido where
    arbitrary = do
        numCards <- choose (20, length baralhoNormal)
        cards <- vectorOf numCards (elements baralhoNormal)
        return (BV cards)

newtype CreditosValidos = CRV Int deriving (Eq, Show)
instance Arbitrary CreditosValidos where
    arbitrary = do
        n <- choose (1, 1000)
        return (CRV n)

-- valor da aposta tem de ser maior que 0 e menor ou igual ao número de créditos
newtype ApostaValida = AV Int deriving (Eq, Show)
instance Arbitrary ApostaValida where
    arbitrary = do
        n <- choose (1, 1000)
        return (AV n)

instance Arbitrary EstadoJogo where
    arbitrary = do
        baralho <- arbitrary :: Gen BaralhoValido
        creditos <- arbitrary :: Gen CreditosValidos
        apostas <- arbitrary :: Gen ApostaValida
        return $ apostar (unAV apostas) (inicializa (unBV baralho) (unCRV creditos))

-- converter baralhoValido para baralho
unBV :: BaralhoValido -> [String]
unBV (BV x) = x
-- converter creditosValidos para creditos
unCRV :: CreditosValidos -> Int
unCRV (CRV x) = x

-- converter apostaValida para aposta
unAV :: ApostaValida -> Int
unAV (AV x) = x