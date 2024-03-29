{-# OPTIONS_GHC -Wno-error=missing-fields -Wno-error=incomplete-patterns -Wno-error=missing-signatures -Wno-error=type-defaults -Wno-error=name-shadowing #-}
{-# Language QuasiQuotes #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns #-}

module Modelling.StateDiagram.AlloyDiagrams (render) where

import Modelling.StateDiagram.Datatype (UMLStateDiagram
                ,unUML
                ,StateDiagram(..)
                ,Connection(..)
                ,HistoryType(..)
                ,globalise)

import Data.String.Interpolate (i)
import Data.List (intercalate)
import Data.List.Extra (nubOrd, notNull)
import Data.Maybe (isNothing, fromJust)

data Inherited = Inherited
  { context :: [Int]
  , nameMapping :: [(String,String)]
  }

data Synthesized = Synthesized
  { alloy :: String
  , names :: [String]
  , rootNodes :: [String]
  , innerStarts :: Int
  , endNodes :: Bool
  , normalStates :: Bool
  , hierarchicalStates :: Bool
  , regionsStates :: Bool
  , deepHistoryNodes :: Bool
  , shallowHistoryNodes :: Bool
  , forkNodes :: Bool
  , joinNodes :: Bool
  }

render :: Int -> UMLStateDiagram String Int -> String
render protoFlowScope =
  unUML (\name substates connection startState ->
  let Synthesized {alloy, names, innerStarts, endNodes, normalStates, hierarchicalStates, regionsStates, deepHistoryNodes, shallowHistoryNodes, forkNodes, joinNodes} =
        renderInner renderNode substates
          Inherited {context = [], nameMapping = nameMapping
                    }
      nameMapping = zipWith (\name -> (name,) . ("Name" ++) . show) (nubOrd names) [1..]
      nameOutput = map (\(_,component) -> [i|one sig #{component} extends ComponentNames{}|])
                   nameMapping
      transitionMapping = zipWith (\name -> (name,) . ("T" ++) . show) (nubOrd (filter (not . null) (map transition connection))) [1..]
      transitionOutput = map (\(_,trigger) -> [i|one sig #{trigger} extends TriggerNames{}|])
                         transitionMapping
      numberOfFlows = length connection + innerStarts + if null startState then 0 else 1
      nullScopes =
        concatMap ((" 0 "++) . (++",\n ") . snd) . filter fst $
        [ (not endNodes, "EndNodes")
        , (null startState && innerStarts == 0, "StartNodes")
        , (null names, "ComponentNames")
        , (null transitionMapping, "TriggerNames")
        , (not normalStates, "NormalStates")
        , (not hierarchicalStates, "HierarchicalStates")
        , (not regionsStates, "RegionsStates")
        , (not regionsStates, "Regions")
        , (not deepHistoryNodes, "DeepHistoryNodes")
        , (not shallowHistoryNodes, "ShallowHistoryNodes")
        , (not forkNodes, "ForkNodes")
        , (not joinNodes, "JoinNodes") ]
  in
  [i|module diagram // name: #{show name}
open uml_state_diagram
#{if null startState then "" else renderStart ("S", startState)}
#{alloy}
#{unlines $ zipWith (renderConnection transitionMapping) connection [1..]}
#{unlines nameOutput}
#{unlines transitionOutput}
run {} for
 #{nullScopes} #{show protoFlowScope} ProtoFlows, exactly #{show numberOfFlows} Flows
|]
        )
  . globalise

renderStart :: (String, [Int]) -> String
renderStart (start, target) = [i|one sig #{start} extends StartNodes{}
one sig #{start}Flow extends Flows{}{
  from = #{start}
  label = EmptyTrigger
  to = N_#{address target}
}|]

renderConnection :: [(String, String)] -> Connection Int -> Int -> String
renderConnection transitionMapping Connection{ pointFrom, pointTo, transition } n = [i|one sig Connection#{n} extends Flows{}{
  from = N_#{address pointFrom}
  label = #{if null transition then "EmptyTrigger" else fromJust (lookup transition transitionMapping) ++ " // " ++ show transition}
  to = N_#{address pointTo}
}|]

renderInner :: (StateDiagram String Int a -> Inherited -> Synthesized) -> [StateDiagram String Int a] -> Inherited -> Synthesized
renderInner recurse substates inherited =
  let
    recursively = map (`recurse` inherited) substates
  in
      Synthesized
      { alloy = unlines $ map alloy recursively
      , names = concatMap names recursively
      , rootNodes = concatMap rootNodes recursively
      , innerStarts = sum (map innerStarts recursively)
      , endNodes = any endNodes recursively
      , normalStates = any normalStates recursively
      , hierarchicalStates = any hierarchicalStates recursively
      , regionsStates = any regionsStates recursively
      , deepHistoryNodes = any deepHistoryNodes recursively
      , shallowHistoryNodes = any shallowHistoryNodes recursively
      , forkNodes = any forkNodes recursively
      , joinNodes = any joinNodes recursively
      }

renderComposite :: String -> (StateDiagram String Int a -> Inherited -> Synthesized) -> StateDiagram String Int a -> Inherited -> Synthesized
renderComposite kind eachWith StateDiagram{ substates, label, name, startState } inherited@Inherited{context, nameMapping} =
  let
    here = context ++ [label]
    node = [i|#{if kind == "Regions" then "R" else "N"}_#{address here}|]
    start = if null startState then Nothing else Just ([i|S_#{address here}|], here ++ startState)
    Synthesized {alloy, names, rootNodes, innerStarts, endNodes, normalStates, hierarchicalStates, regionsStates, deepHistoryNodes, shallowHistoryNodes, forkNodes, joinNodes} =
      renderInner eachWith substates
        inherited {context = here}
  in
  Synthesized
  { alloy = unlines $
            [i|one sig #{node} extends #{kind}{}{#{if kind == "RegionsStates" then "" else if null name then noName else "\n  name = " ++ fromJust (lookup name nameMapping) ++ " // " ++ show name}
  contains = #{intercalate " + " (maybe [] (\x -> [fst x]) start ++ rootNodes)}
}|]
            : maybe id ((:) . renderStart) start
            [ alloy ]
  , names = if null name then names else name : names
  , rootNodes = [node]
  , innerStarts = innerStarts + if isNothing start then 0 else 1
  , endNodes = endNodes
  , normalStates = normalStates
  , hierarchicalStates = kind == "HierarchicalStates" || hierarchicalStates
  , regionsStates = kind == "RegionsStates" || regionsStates
  , deepHistoryNodes = deepHistoryNodes
  , shallowHistoryNodes = shallowHistoryNodes
  , forkNodes = forkNodes
  , joinNodes = joinNodes
  }
  where
  noName = "\n  no name"  -- because the line length limit is 200 characters
renderComposite _ _ _ _ = error "not defined"

defaultSynthesized :: Synthesized
defaultSynthesized = Synthesized
  { names = []
  , innerStarts = 0
  , endNodes = False
  , normalStates = False
  , hierarchicalStates = False
  , regionsStates = False
  , deepHistoryNodes = False
  , shallowHistoryNodes = False
  , forkNodes = False
  , joinNodes = False
  }

renderNode :: StateDiagram String Int a -> Inherited -> Synthesized

renderNode d@StateDiagram{} inherited =
  renderComposite "HierarchicalStates" renderNode d inherited

renderNode CombineDiagram { substates, label } inherited =
  renderComposite "RegionsStates"
    (renderComposite "Regions" renderNode)
    StateDiagram{ substates = substates, label = label, name = "", startState = [] }
    inherited

renderNode InnerMostState { label, name } Inherited{context, nameMapping} =
  let
    here = context ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends NormalStates{}{
  #{if null name then "no name" else "name = " ++ fromJust (lookup name nameMapping) ++ " // " ++ show name}
}|]
  , names = [name | notNull name]
  , rootNodes = [node]
  , normalStates = True
  }

renderNode EndState { label } Inherited{context} =
  let
    here = context ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends EndNodes{}|]
  , rootNodes = [node]
  , endNodes = True
  }

renderNode History { label, historyType } Inherited{context} =
  let
    here = context ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends #{show historyType}HistoryNodes{}|]
  , rootNodes = [node]
  , deepHistoryNodes = historyType == Deep
  , shallowHistoryNodes = historyType == Shallow
  }

renderNode Fork {label} Inherited {context} =
  let
    here = context ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends ForkNodes{}|]
  , rootNodes = [node]
  , forkNodes = True
  , joinNodes = False
  }

renderNode Join {label} Inherited {context} =
  let
    here = context ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends JoinNodes{}|]
  , rootNodes = [node]
  , forkNodes = False
  , joinNodes = True
  }

address :: [Int] -> String
address = intercalate "_" . map show
