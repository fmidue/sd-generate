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

renderAll :: UMLStateDiagram String Int -> String
renderAll sd =
  let
    info = "/'name: #{show name} (irrelevant) label: #{show label}'/"
    StateDiagram{ connections = connections } = unUML' sd
    inherited = Inherited{ctxt = [], connectionSources = map (\Connection{ pointFrom } -> [i|N_#{address "" pointFrom}|]) connections}
  in
    [i|@startuml
#{info}

#{renderUML sd inherited}
@enduml
|]

renderUML :: UMLStateDiagram String Int -> Inherited -> String
renderUML sd inherited =
  let
    StateDiagram{ substates, connections, startState } = unUML' sd
  in
  case unUML' (globalise sd) of
    StateDiagram subst _ _ _ _ ->   let substates1 = subst
                                        hn = getAllHistory substates1 inherited
                                    in
                                    [i|
#{renderSubstates substates inherited}
#{renderStart startState inherited}
#{renderConnections hn connections inherited}|]
    _ -> error "not defined"

renderSubstates :: [StateDiagram String Int [Connection Int]] -> Inherited -> String
renderSubstates [] _ = []
renderSubstates (x:xs) inherited@Inherited{ ctxt, connectionSources } =
  case x of

    StateDiagram{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|]
        ++ "{\n" ++ renderUML (umlStateDiagram x) Inherited{ctxt=ctxt ++ [label], connectionSources} ++ "}\n"

    CombineDiagram{ substates, label } ->
        [i|state "RegionsState" as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "{\n"
        ++ renderRegions substates Inherited{ctxt=ctxt ++ [label], connectionSources} ++ "}\n"

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
  ++ renderSubstates xs inherited

renderRegions :: [StateDiagram String Int [Connection Int]] -> Inherited -> String
renderRegions [] _ = []
renderRegions (r:rs) inherited@Inherited{ ctxt, connectionSources } =
  case r of
    StateDiagram{ label } ->
        renderUML (umlStateDiagram r) Inherited{ctxt=ctxt ++ [label], connectionSources}
        ++ if null rs then "" else "--\n"

    CombineDiagram{ substates, label } ->
        renderRegions substates Inherited{ctxt=ctxt ++ [label], connectionSources}

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

renderConnections :: [(StateDiagram String Int [Connection Int], [Int])] -> [Connection Int] -> Inherited -> String
renderConnections _ [] _ = []
renderConnections [] (Connection{ pointFrom, pointTo, transition }:cs) inherited@Inherited{ ctxt } =
  let
    here_pointFrom = ctxt ++ pointFrom
    here_pointTo = ctxt ++ pointTo
  in
    [i|N_#{address "" here_pointFrom} --> N_#{address "" here_pointTo}#{if null transition then "\n" else " : " ++ transition ++ "\n"}|]
    ++ renderConnections [] cs inherited
renderConnections hn@((History{ historyType },completeLabel):hs) cx@(Connection{ pointFrom, pointTo, transition }:cs) inherited@Inherited{ ctxt }
  | completeLabel == here_pointFrom    = from_hc ++ renderConnections hn cs inherited
  | completeLabel == here_pointTo      = to_hc ++ renderConnections hn cs inherited
  | otherwise                          = let
                                           str = if null hs then [] else renderConnections hs cx inherited
                                         in
                                           if null str then oc ++ renderConnections hn cs inherited else str
    where
      here_pointFrom = ctxt ++ pointFrom
      here_pointTo = ctxt ++ pointTo
      isNullTransition = {-if null transition then "->" else-} "-->"
      isHistoryType = if historyType == Shallow then "[H]" else "[H*]"
      transitionLabel = if null transition then "\n" else " : " ++ transition ++ "\n"
      oc = [i|N_#{address "" here_pointFrom} #{isNullTransition} N_#{address "" here_pointTo}#{transitionLabel}|]
      from_hc = [i|#{isHistoryType} --> N_#{address "" here_pointTo}|] ++ "\n"
      to_hc = [i|N_#{address "" here_pointFrom} #{isNullTransition} N_#{address isHistoryType here_pointTo}#{transitionLabel}|] ++ "\n"
renderConnections _ _ _ = error "not defined"

address :: String -> [Int] -> String
address "" as = intercalate "_" (map show as)
address h as = intercalate "_" (map show (init as)) ++ h

getAllHistory :: [StateDiagram String Int [Connection Int]] -> Inherited -> [(StateDiagram String Int [Connection Int], [Int])]
getAllHistory [] _ = []
getAllHistory (x:xs) inherited@Inherited{ ctxt, connectionSources } =
  case x of
    StateDiagram{ substates, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substates Inherited{ctxt=here, connectionSources}

    CombineDiagram{ substates, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substates Inherited{ctxt=here, connectionSources}

    History{label} -> [(x, ctxt ++ [label])]

    _ -> []
  ++ getAllHistory xs inherited
