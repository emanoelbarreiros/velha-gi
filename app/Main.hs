{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad (void)
import GI.Gtk (Button (..), Grid (..), Image (..), Window (..), Label(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.Container.Grid
import Data.Text
import Lib
import Velha

data Estado = Estado
  { tabuleiro :: [Jogada],
    situacao :: Situacao,
    proximaJogada :: Valor
  }

data Evento = JogadaRealizada Int Int | Fechar

view' :: Estado -> AppView Window Evento
view' s =
  bin
    Window
    [#title := "Velha", #resizable := False, on #deleteEvent (const (True, Fechar))]
    $ container
        Grid
        [#rowSpacing := 6, #columnSpacing := 5, #margin := 1]
        [ GridChild --linha 1
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 0, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 0 0)] $ widget Image [#file := obterImagem s 0]
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 1, topAttach = 0, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 2, topAttach = 0, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 0 1)] $ widget Image [#file := obterImagem s 1]              
            } ,
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 3, topAttach = 0, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 4, topAttach = 0, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 0 2)] $ widget Image [#file := obterImagem s 2]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 1, width = 5, height = 1} ,
              child = widget Image [#file := "segmentohorizontal.jpg"]              
            },
          
          GridChild --linha 2
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 2, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 1 0)] $ widget Image [#file := obterImagem s 3]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 1, topAttach = 2, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 2, topAttach = 2, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 1 1)] $ widget Image [#file := obterImagem s 4]              
            } ,
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 3, topAttach = 2, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 4, topAttach = 2, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 1 2)] $ widget Image [#file := obterImagem s 5]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 3, width = 5, height = 1} ,
              child = widget Image [#file := "segmentohorizontal.jpg"]              
            },

          GridChild --linha 3
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 4, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 2 0)] $ widget Image [#file := obterImagem s 6]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 1, topAttach = 4, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 2, topAttach = 4, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 2 1)] $ widget Image [#file := obterImagem s 7]              
            } ,
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 3, topAttach = 4, width = 1, height = 1} ,
              child = widget Image [#file := "segmentovertical.jpg"]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 4, topAttach = 4, width = 1, height = 1} ,
              child = bin Button [on #clicked (JogadaRealizada 2 2)] $ widget Image [#file := obterImagem s 8]              
            },
          GridChild 
            { properties = defaultGridChildProperties {leftAttach = 0, topAttach = 5, width = 5, height = 1} ,
              child = widget Label [#label := obterMensagem s]              
            }
        ]
    

update' :: Estado -> Evento -> Transition Estado Evento
update' est evt =
  case evt of
    Fechar -> Exit
    JogadaRealizada x y -> Transition est {tabuleiro = tab, situacao = obterSituacao tab, proximaJogada = prox } (pure Nothing)
                           where
                               resultado = decidirJogada est (x,y)
                               tab = fst resultado
                               prox = snd resultado


obterMensagem :: Estado -> Text 
obterMensagem e =
    case situacao e of
        VencedorX -> "Vitória do jogador X."
        VencedorO -> "Vitória do jogador O."
        Jogando -> "Vez do jogador " <> pack (show (proximaJogada e)) <> "."
        Empate -> "Empate."


obterImagem :: Estado -> Int -> Text
obterImagem s i = 
    case snd (tabuleiro s !! i) of
        X -> "x.jpg"
        O -> "o.jpg"
        N -> "n.jpg"


decidirJogada :: Estado -> Posicao -> (Tabuleiro, Valor)
decidirJogada e p = 
    case situacao e of
        VencedorX -> (tabuleiro e, Inv)
        VencedorO -> (tabuleiro e, Inv)
        Empate -> (tabuleiro e, Inv)
        Jogando -> jogar p (proximaJogada e) (situacao e) (tabuleiro e)

main :: IO ()
main =
  void $
    run App {
        view = view', 
        update = update', 
        inputs = [], 
        initialState = Estado {tabuleiro = tabVazio, situacao = Jogando, proximaJogada = O}
    }
