module Velha where

import Data.Maybe
import Data.List.Split
import Lib


type Posicao = (Int,Int)

type Jogada = (Posicao,Valor)

data Valor = X | O | N | Inv deriving (Show, Eq)

data Linha = H1 | H2 | H3 | V1 | V2 | V3 | DP | DS

type Tabuleiro = [Jogada]

data Situacao = VencedorX | VencedorO | Jogando | Empate deriving (Eq, Show)

tabVazio :: Tabuleiro
tabVazio = [((0,0),N), ((0,1),N), ((0,2),N),
            ((1,0),N), ((1,1),N), ((1,2),N),
            ((2,0),N), ((2,1),N), ((2,2),N)]

-- rodarJogo :: Tabuleiro -> IO ()
-- rodarJogo tab = do
--     if semVencedor tab then do
--         if contarJogadas N tab > 0 then do
--             let vez = rodada tab
--             putStr ("Vez do " ++ show vez ++ ". Informe a jogada: ")
--             jogada <- obterString
--             let posicao = convertePosicao jogada
--             if isJust posicao && vazia (fromJust posicao) tab then do
--                 let novoTab = jogar (fromJust posicao) vez tab
--                 printTabuleiro novoTab
--                 rodarJogo novoTab
--             else do
--                 putStrLn "Jogada inválida, tente novamente."
--                 rodarJogo tab
--         else do
--             putStrLn "Empate."
--     else do
--         if vencedorX tab then putStrLn "Vitória do jogador X." else putStrLn "Vitória do jogador O."

        
-- convertePosicao :: String -> Maybe Posicao
-- convertePosicao s = if length partes == 2 then
--                         Just (read (head partes) :: Int, read (partes !! 1) :: Int)
--                     else
--                         Nothing
--                     where
--                         partes = splitOn " " s



rodada :: Tabuleiro -> Valor
rodada tab = if x_s < o_s then X else O
             where 
                 x_s = contarJogadas X tab
                 o_s = contarJogadas O tab

jogar :: Posicao -> Valor -> Situacao -> Tabuleiro -> (Tabuleiro, Valor)
jogar pos val sit tab = 
    if vazia pos tab && sit == Jogando then (resultado, outroJogador val) else (tab, val)
    where 
        resultado = map (aplicar (pos,val)) tab

outroJogador :: Valor -> Valor
outroJogador X = O
outroJogador O = X

obterSituacao :: Tabuleiro -> Situacao
obterSituacao tab
    | vencedorX tab = VencedorX
    | vencedorO tab = VencedorO
    | contarJogadas N tab > 0 = Jogando
    | otherwise = Empate

vazia :: Posicao -> Tabuleiro -> Bool 
vazia pos tab = valor == N
                where
                    valor = obterValor pos tab

valor :: Jogada -> Valor
valor (p,v) = v

aplicar :: Jogada -> Jogada -> Jogada
aplicar ((x,y),v) ((x',y'),v') = if x == x' && y == y' then ((x,y),v) else ((x',y'),v')

obterValor :: Posicao -> Tabuleiro -> Valor
obterValor pos tab = fromMaybe Inv (lookup pos tab)

posicoes :: Linha -> [Posicao]
posicoes H1 = [(0,0),(0,1),(0,2)]
posicoes H2 = [(1,0),(1,1),(1,2)]
posicoes H3 = [(2,0),(2,1),(2,2)]
posicoes V1 = [(0,0),(1,0),(2,0)]
posicoes V2 = [(0,1),(1,1),(2,1)]
posicoes V3 = [(0,2),(1,2),(2,2)]
posicoes DP = [(0,0),(1,1),(2,2)]
posicoes DS = [(0,2),(1,1),(2,0)]

filtrarPosicoes :: [Posicao] -> Jogada -> Bool
filtrarPosicoes ps (p,v) = p `elem` ps

obterLinha :: Tabuleiro -> Linha -> [Valor]
obterLinha tab lin = map valor $ filter (filtrarPosicoes (posicoes lin)) tab

venceu :: Valor -> Tabuleiro -> Bool
venceu v tab = any (all (== v) . obterLinha tab) [H1, H2, H3, V1, V2, V3, DP, DS]

vencedorX :: Tabuleiro -> Bool
vencedorX = venceu X

vencedorO :: Tabuleiro -> Bool 
vencedorO = venceu O

semVencedor :: Tabuleiro -> Bool 
semVencedor tab = not (vencedorO tab) && not (vencedorX tab)

contarJogadas :: Valor -> Tabuleiro -> Int
contarJogadas v = foldr ((+) . contar v . valor) 0

contar :: Valor -> Valor -> Int
contar v o = if v == o then 1 else 0

printTabuleiro :: Tabuleiro -> IO ()
printTabuleiro [] = return ()
printTabuleiro (((x,y),v):xs) = do
    if y == 2 then do
        print v
    else do
        putStr (show v ++ " | ")
    printTabuleiro xs