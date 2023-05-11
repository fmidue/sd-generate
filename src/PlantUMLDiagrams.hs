{-# OPTIONS_GHC -Wno-error=missing-fields -Wno-error=incomplete-patterns -Wno-error=missing-signatures -Wno-error=type-defaults -Wno-error=name-shadowing #-}
{-# Language QuasiQuotes    #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns   #-}

module PlantUMLDiagrams (renderAll) where

import Datatype (UMLStateDiagram
                ,StateDiagram(..)
                ,Connection(..)
                ,HistoryType(..)
                ,globalise)

import Data.String.Interpolate (i)
import Data.List (intercalate)

data Inherited = Inherited { ctxt :: [Int]
                           , connectionFroms :: [String] }

renderAll :: UMLStateDiagram -> String
renderAll sd@StateDiagram{} =
  let
    info = "/'name: #{show name} (irrelevant) label: #{show label}'/"
    StateDiagram{ connection=connection } = sd
    inherited = Inherited{ctxt = [], connectionFroms = map (\Connection{ pointFrom } -> [i|N_#{address "" pointFrom}|]) connection}
  in
    [i|@startuml
#{info}

#{renderUML sd inherited}
@enduml
|]
renderAll _ = error "not defined"

renderUML :: UMLStateDiagram -> Inherited -> String
renderUML sd@StateDiagram{ substate, connection, startState } inherited =
  case globalise sd of
    StateDiagram subst _ _ _ _ ->   let substate1 = subst
                                        hn = getAllHistory substate1 inherited
                                    in
                                    [i|
#{renderSubState substate inherited}
#{renderStart startState inherited}
#{renderConnection hn connection inherited}|]
    _ -> error "not defined"
renderUML _ _ = error "not defined"


renderSubState :: [UMLStateDiagram] -> Inherited -> String
renderSubState [] _ = []
renderSubState (x:xs) inherited@Inherited{ ctxt, connectionFroms } =
  case x of

    StateDiagram{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|]
        ++ "{\n" ++ renderUML x Inherited{ctxt=ctxt ++ [label], connectionFroms} ++ "}\n"

    CombineDiagram{ substate, label } ->
        [i|state "RegionsState" as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "{\n"
        ++ renderRegions substate Inherited{ctxt=ctxt ++ [label], connectionFroms} ++ "}\n"

    EndState { label } ->
        [i|state #{("N_" ++ (address "" (ctxt ++ [label])))} <<end>>|] ++ "\n"

    InnerMostState{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "\n"

    History {} -> ""

    Joint { label } ->
      let
        node = ("N_" ++ address "" (ctxt ++ [label]))
        isFork = length (filter (node==) connectionFroms) > 1
      in
        [i|state #{node} #{if isFork then "<<fork>>" else "<<join>>"}|] ++ "\n"
  ++ renderSubState xs inherited

renderRegions :: [UMLStateDiagram] -> Inherited -> String
renderRegions [] _ = []
renderRegions (r:rs) inherited@Inherited{ ctxt, connectionFroms } =
  case r of
    StateDiagram{ label } ->
        renderUML r Inherited{ctxt=ctxt ++ [label], connectionFroms}
        ++ if null rs then "" else "--\n"

    CombineDiagram{ substate, label } ->
        renderRegions substate Inherited{ctxt=ctxt ++ [label], connectionFroms}

    EndState { label } ->
        [i|state #{("N_" ++ (address "" (ctxt ++ [label])))} <<end>>|] ++ "\n"

    InnerMostState{ label, name } ->
        [i|state #{if null name then "\"" ++ "EmptyName" ++ "\"" else show name} as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "\n"

    History {} -> ""

    Joint { label } ->
      let
        node = "N_" ++ address "" (ctxt ++ [label])
        isFork = length (filter (node==) connectionFroms) > 1
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

renderConnection :: [(UMLStateDiagram, [Int])] -> [Connection] -> Inherited -> String
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

getAllHistory :: [UMLStateDiagram] -> Inherited -> [(UMLStateDiagram, [Int])]
getAllHistory [] _ = []
getAllHistory (x:xs) inherited@Inherited{ ctxt, connectionFroms } =
  case x of
    StateDiagram{ substate, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substate Inherited{ctxt=here, connectionFroms}

    CombineDiagram{ substate, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substate Inherited{ctxt=here, connectionFroms}

    History{label} -> [(x, ctxt ++ [label])]

    _ -> []
  ++ getAllHistory xs inherited
