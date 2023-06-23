{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# OPTIONS_GHC -Wno-error=missing-fields -Wno-error=incomplete-patterns -Wno-error=incomplete-uni-patterns -Wno-error=missing-signatures -Wno-error=type-defaults -Wno-error=name-shadowing #-}
{-# Language QuasiQuotes    #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns   #-}

module PlantUMLDiagrams (renderAll) where

import Datatype (UMLStateDiagram(unUML')
                ,umlStateDiagram
                ,StateDiagram(..)
                ,Connection(..)
                ,HistoryType(..)
                ,globalise)

import Data.String.Interpolate (i)
import Data.List (intercalate)

data Inherited = Inherited { ctxt :: [Int]
                           , connectionSources :: [String] }

renderAll :: UMLStateDiagram Int -> String
renderAll sd =
  let
    info = "/'name: #{show name} (irrelevant) label: #{show label}'/"
    StateDiagram{ connection=connection } = unUML' sd
    inherited = Inherited{ctxt = [], connectionSources = map (\Connection{ pointFrom } -> [i|N_#{address "" pointFrom}|]) connection}
  in
    [i|@startuml
#{info}

#{renderUML sd inherited}
@enduml
|]

renderUML :: UMLStateDiagram Int -> Inherited -> String
renderUML sd inherited =
  let
    StateDiagram{ substate, connection, startState } = unUML' sd
  in
  case unUML' (globalise sd) of
    StateDiagram subst _ _ _ _ ->   let substate1 = subst
                                        hn = getAllHistory substate1 inherited
                                    in
                                    [i|
#{renderSubState substate inherited}
#{renderStart startState inherited}
#{renderConnection hn connection inherited}|]
    _ -> error "not defined"

renderSubState :: [StateDiagram Int [Connection Int]] -> Inherited -> String
renderSubState [] _ = []
renderSubState (x:xs) inherited@Inherited{ ctxt, connectionSources } =
  case x of

    StateDiagram{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|]
        ++ "{\n" ++ renderUML (umlStateDiagram x) Inherited{ctxt=ctxt ++ [label], connectionSources} ++ "}\n"

    CombineDiagram{ substate, label } ->
        [i|state "RegionsState" as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "{\n"
        ++ renderRegions substate Inherited{ctxt=ctxt ++ [label], connectionSources} ++ "}\n"

    EndState { label } ->
        [i|state #{("N_" ++ (address "" (ctxt ++ [label])))} <<end>>|] ++ "\n"

    InnerMostState{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "\n"

    History {} -> ""

    Joint { label } ->
      let
        node = ("N_" ++ address "" (ctxt ++ [label]))
        isFork = length (filter (node==) connectionSources) > 1
      in
        [i|state #{node} #{if isFork then "<<fork>>" else "<<join>>"}|] ++ "\n"
  ++ renderSubState xs inherited

renderRegions :: [StateDiagram Int [Connection Int]] -> Inherited -> String
renderRegions [] _ = []
renderRegions (r:rs) inherited@Inherited{ ctxt, connectionSources } =
  case r of
    StateDiagram{ label } ->
        renderUML (umlStateDiagram r) Inherited{ctxt=ctxt ++ [label], connectionSources}
        ++ if null rs then "" else "--\n"

    CombineDiagram{ substate, label } ->
        renderRegions substate Inherited{ctxt=ctxt ++ [label], connectionSources}

    EndState { label } ->
        [i|state #{("N_" ++ (address "" (ctxt ++ [label])))} <<end>>|] ++ "\n"

    InnerMostState{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "\n"

    History {} -> ""

    Joint { label } ->
      let
        node = "N_" ++ address "" (ctxt ++ [label])
        isFork = length (filter (node==) connectionSources) > 1
      in
        [i|state #{node} #{if isFork then "<<fork>>" else "<<join>>"}|] ++ "\n"
  ++ renderRegions rs inherited

renderStart :: [Int] -> Inherited -> String
renderStart [] _ = []
renderStart target Inherited{ctxt} =
  let
    here_target = ctxt ++ target
  in
    [i|[*] -> N_#{address "" here_target}|] ++ "\n"

renderConnection :: [(StateDiagram Int [Connection Int], [Int])] -> [Connection Int] -> Inherited -> String
renderConnection _ [] _ = []
renderConnection [] (Connection{ pointFrom, pointTo, transition }:cs) inherited@Inherited{ ctxt } =
  let
    here_pointFrom = ctxt ++ pointFrom
    here_pointTo = ctxt ++ pointTo
  in
    [i|N_#{address "" here_pointFrom} --> N_#{address "" here_pointTo}#{if null transition then "\n" else " : " ++ transition ++ "\n"}|]
    ++ renderConnection [] cs inherited
renderConnection hn@((History{ historyType },completeLabel):hs) cx@(Connection{ pointFrom, pointTo, transition }:cs) inherited@Inherited{ ctxt }
  | completeLabel == here_pointFrom    = from_hc ++ renderConnection hn cs inherited
  | completeLabel == here_pointTo      = to_hc ++ renderConnection hn cs inherited
  | otherwise                          = let
                                           str = if null hs then [] else renderConnection hs cx inherited
                                         in
                                           if null str then oc ++ renderConnection hn cs inherited else str
    where
      here_pointFrom = ctxt ++ pointFrom
      here_pointTo = ctxt ++ pointTo
      isNullTransition = {-if null transition then "->" else-} "-->"
      isHistoryType = if historyType == Shallow then "[H]" else "[H*]"
      transitionLabel = if null transition then "\n" else " : " ++ transition ++ "\n"
      oc = [i|N_#{address "" here_pointFrom} #{isNullTransition} N_#{address "" here_pointTo}#{transitionLabel}|]
      from_hc = [i|#{isHistoryType} --> N_#{address "" here_pointTo}|] ++ "\n"
      to_hc = [i|N_#{address "" here_pointFrom} #{isNullTransition} N_#{address isHistoryType here_pointTo}#{transitionLabel}|] ++ "\n"
renderConnection _ _ _ = error "not defined"

address :: String -> [Int] -> String
address "" as = intercalate "_" (map show as)
address h as = intercalate "_" (map show (init as)) ++ h

getAllHistory :: [StateDiagram Int [Connection Int]] -> Inherited -> [(StateDiagram Int [Connection Int], [Int])]
getAllHistory [] _ = []
getAllHistory (x:xs) inherited@Inherited{ ctxt, connectionSources } =
  case x of
    StateDiagram{ substate, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substate Inherited{ctxt=here, connectionSources}

    CombineDiagram{ substate, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substate Inherited{ctxt=here, connectionSources}

    History{label} -> [(x, ctxt ++ [label])]

    _ -> []
  ++ getAllHistory xs inherited
