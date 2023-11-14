{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-# OPTIONS_GHC -Wno-error=missing-fields -Wno-error=incomplete-patterns -Wno-error=incomplete-uni-patterns -Wno-error=missing-signatures -Wno-error=type-defaults -Wno-error=name-shadowing #-}
{-# Language QuasiQuotes    #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns   #-}

module Modelling.StateDiagram.PlantUMLDiagrams (renderAll
                                               ,drawSDToFile) where

import Modelling.StateDiagram.Datatype
                (UMLStateDiagram
                ,unUML
                ,umlStateDiagram
                ,StateDiagram(..)
                ,Connection(..)
                ,HistoryType(..)
                )
import Modelling.StateDiagram.Style (Styling(..))
import Test.QuickCheck (elements, suchThat, vectorOf, generate, infiniteListOf)

import Data.String.Interpolate (i)
import Data.List (intercalate)
import Data.List.Extra (nubOrd)
import Language.PlantUML.Call (drawPlantUMLDiagram, DiagramType (SVG))
import Data.ByteString.Char8 (pack, unpack)

renderAll :: Styling -> UMLStateDiagram String Int -> IO String
renderAll style sd = do
  colors <- generate (infiniteListOf (vectorOf 3 (vectorOf 2 (elements "0123456789abcdef")) `suchThat` (\[r,g,b] -> r /= g || g /= b)))
  let
    info = "/'name: #{show name} (irrelevant) label: #{show label}'/"
    ctxt = []
    (rendered, relevantNames) = renderUML style ctxt sd
    theStyling =
      case style of
        Unstyled
          -> ""

        _
          -> "<style>\n" ++
             concatMap (\(n,[r,g,b]) ->
                           let c = r ++ g ++ b
                           in
                             '.' : n ++ " {\n  FontColor #" ++ c ++ "\n  LineColor #" ++ c ++ "\n}\n")
             (zip relevantNames (if style == StyledRainbow then nubOrd colors else repeat ["00","00","00"]))
             ++ "</style>\n"
  return [i|@startuml
#{theStyling}
#{info}

#{rendered}
@enduml
|]

renderUML :: Styling -> [Int] -> UMLStateDiagram String Int -> (String, [String])
renderUML style ctxt =
  unUML $ \_ substates connections startState ->
                                    let hn = getAllHistory substates ctxt
                                        (theSubstates, relevantNames) = renderSubstates substates style ctxt
                                    in
                                    ([i|
#{theSubstates}
#{renderStart startState ctxt}
#{renderConnections hn connections ctxt}|], relevantNames)

renderSubstates :: [StateDiagram String Int [Connection Int]] -> Styling -> [Int] -> (String, [String])
renderSubstates [] _ _ = ("",[])
renderSubstates (x:xs) style ctxt =
  let (rest, restNames) = renderSubstates xs style ctxt in
  case x of

    StateDiagram{ label, name } ->
       ([i|state #{theName} as #{("N_" ++ (address "" (ctxt ++ [label])))}#{theStyle}|]
        ++ "{\n" ++ recursively ++ "}\n"
        ++ rest, name : namesRecursively ++ restNames)
        where
          theName = if null name then "\"" ++ "EmptyName" ++ "\"" else show name
          theStyle = if null name || style == Unstyled then "" else " <<" ++ name ++ ">>"
          (recursively, namesRecursively) = renderUML style (ctxt ++ [label]) (umlStateDiagram x)

    CombineDiagram{ substates, label } ->
       ([i|state "RegionsState" as #{("N_" ++ (address "" (ctxt ++ [label])))}|] ++ "{\n"
        ++ recursively ++ "}\n"
        ++ rest, namesRecursively ++ restNames)
        where
          (recursively, namesRecursively) = renderRegions substates style (ctxt ++ [label])

    EndState { label } ->
       ([i|state #{("N_" ++ (address "" (ctxt ++ [label])))} <<end>>|] ++ "\n" ++ rest, restNames)

    InnerMostState{ label, name } ->
       ([i|state #{theName} as #{("N_" ++ (address "" (ctxt ++ [label])))}#{theStyle}|] ++ "\n"
        ++ rest, name : restNames)
        where
          theName = if null name then "\"" ++ "EmptyName" ++ "\"" else show name
          theStyle = if null name || style == Unstyled then "" else " <<" ++ name ++ ">>"

    History {} -> (rest, restNames)

    Fork { label }
      -> let node = ("N_" ++ address "" (ctxt ++ [label])) in
         ([i|state #{node} <<fork>>|] ++ "\n" ++ rest, restNames)

    Join { label }
      -> let node = ("N_" ++ address "" (ctxt ++ [label])) in
         ([i|state #{node} <<join>>|] ++ "\n" ++ rest, restNames)

renderRegions :: [StateDiagram String Int [Connection Int]] -> Styling -> [Int] -> (String, [String])
renderRegions [] _ _ = ("", [])
renderRegions (r:rs) style ctxt =
  let (rest, restNames) = renderRegions rs style ctxt in
  case r of
    StateDiagram{ label } ->
        (recursively ++ if null rs then "" else "--\n" ++ rest, namesRecursively ++ restNames)
        where
          (recursively, namesRecursively) = renderUML style (ctxt ++ [label]) (umlStateDiagram r)

    _ -> error "impossible!"

renderStart :: [Int] -> [Int] -> String
renderStart [] _ = []
renderStart target ctxt =
  let
    here_target = ctxt ++ target
  in
    [i|[*] -> N_#{address "" here_target}|] ++ "\n"

renderConnections :: [(StateDiagram String Int [Connection Int], [Int])] -> [Connection Int] -> [Int] -> String
renderConnections _ [] _ = []
renderConnections [] (Connection{ pointFrom, pointTo, transition }:cs) ctxt =
  let
    here_pointFrom = ctxt ++ pointFrom
    here_pointTo = ctxt ++ pointTo
  in
    [i|N_#{address "" here_pointFrom} --> N_#{address "" here_pointTo}#{if null transition then "\n" else " : " ++ transition ++ "\n"}|]
    ++ renderConnections [] cs ctxt
renderConnections hn@((History{ historyType },completeLabel):hs) cx@(Connection{ pointFrom, pointTo, transition }:cs) ctxt
  | completeLabel == here_pointFrom    = from_hc ++ renderConnections hn cs ctxt
  | completeLabel == here_pointTo      = to_hc ++ renderConnections hn cs ctxt
  | otherwise                          = let
                                           str = if null hs then [] else renderConnections hs cx ctxt
                                         in
                                           if null str then oc ++ renderConnections hn cs ctxt else str
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

getAllHistory :: [StateDiagram String Int a] -> [Int] -> [(StateDiagram String Int a, [Int])]
getAllHistory [] _ = []
getAllHistory (x:xs) ctxt =
  case x of
    StateDiagram{ substates, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substates here

    CombineDiagram{ substates, label } ->
      let
        here = ctxt ++ [label]
      in
        getAllHistory substates here

    History{label} -> [(x, ctxt ++ [label])]

    _ -> []
  ++ getAllHistory xs ctxt

drawSDToFile :: FilePath -> UMLStateDiagram String Int -> IO FilePath
drawSDToFile path chart
  = do
    rendered <- renderAll Unstyled chart
    let plantUML = pack rendered
    picture <- drawPlantUMLDiagram SVG plantUML
    writeFile (path ++ "Diagram.svg") (unpack picture)
    return (path ++ "Diagram.svg")
