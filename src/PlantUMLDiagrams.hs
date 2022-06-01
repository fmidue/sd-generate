{-# OPTIONS_GHC -Wno-error=missing-fields -Wno-error=incomplete-patterns -Wno-error=missing-signatures -Wno-error=type-defaults -Wno-error=name-shadowing #-}
{-# Language QuasiQuotes #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns #-}

module PlantUMLDiagrams (renderAll) where

import Datatype (UMLStateDiagram, StateDiagram(..), Connection(..), HistoryType(..))

import Data.String.Interpolate (i)
import Data.List (intercalate)

data Inherited = Inherited
  { ctxt :: [Int]
  , connectionFroms :: [String]
  }

renderAll :: UMLStateDiagram -> String
renderAll sd = 
  let 
    info = "/'name: #{show name} (irrelevant) label: #{show label}'/"
    StateDiagram{connection=connection} = sd
    inherited = Inherited{ctxt = [], connectionFroms = map (\Connection{ pointFrom } -> [i|N_#{address pointFrom}|]) connection}
  in
    [i|@startuml
#{info}
skinparam state<<history>> {
    backgroundColor transparent
    borderColor transparent
    fontSize 25
}
#{renderUML sd inherited}
|]

renderUML :: UMLStateDiagram -> Inherited -> String
renderUML StateDiagram{substate, connection, startState} inherited =
  [i|
#{renderStart startState inherited}
#{renderSubState substate inherited}
#{renderConnection connection inherited}|]

renderSubState :: [UMLStateDiagram] -> Inherited -> String
renderSubState [] _ = []
renderSubState (x:xs) inherited@Inherited{ctxt, connectionFroms} = 
  case x of
    StateDiagram{label, name} ->
      let
        here = ctxt ++ [label]
        node = [i|N_#{address here}|] ++ ""
      in
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{node}|] 
        ++ "{\n" ++ renderUML x Inherited{ctxt=here, connectionFroms} ++ "}\n"

    CombineDiagram{{--substate, label--}} -> "" -- not finished yet

    EndState { label } -> 
      let
        here = ctxt ++ [label]
        node = [i|N_#{address here}|] ++ ""
      in
        [i|state #{node} <<end>>|] ++ "\n"

    InnerMostState{ label, name } ->
      let
        here = ctxt ++ [label]
        node = [i|N_#{address here}|] ++ ""
      in
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{node}|] ++ "\n"

    History { label, historyType } -> 
      let
        here = ctxt ++ [label]
        node = [i|N_#{address here}|] ++ ""
      in
        [i|state #{if historyType == Shallow then "\"" ++ "[H]" ++ "\"" else "\"" ++ "[H*]" ++ "\""} as #{node} <<history>>|] ++ "\n"

    Joint { label } -> 
      let
        here = ctxt ++ [label]
        node = [i|N_#{address here}|] ++ ""
        isFork = length (filter (node==) connectionFroms) > 1
      in
        [i|state #{node} #{if isFork then "<<fork>>" else "<<join>>"}|] ++ "\n"
  ++ renderSubState xs inherited

renderStart :: [Int] -> Inherited -> String
renderStart target Inherited{ctxt} =
  let
    here_target = ctxt ++ target
  in
    [i|[*] -> N_#{address here_target}|] ++ "\n"

renderConnection :: [Connection] -> Inherited -> String
renderConnection [] _ = []
renderConnection (Connection{ pointFrom, pointTo, transition } :cs) inherited@Inherited{ctxt} = 
  let
    here_pointFrom = ctxt ++ pointFrom
    here_pointTo = ctxt ++ pointTo
  in
    [i|N_#{address here_pointFrom} #{if null transition then "->" else "-->"} N_#{address here_pointTo} #{if null transition then "\n" else ": " ++ transition ++ "\n"}|]
    ++ renderConnection cs inherited

address :: [Int] -> String
address = intercalate "_" . map show