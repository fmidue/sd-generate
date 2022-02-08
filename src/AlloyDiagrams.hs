{-# OPTIONS_GHC -Wno-missing-fields -Wno-incomplete-patterns -Wno-missing-signatures -Wno-type-defaults -Wno-name-shadowing #-}
{-# Language QuasiQuotes #-}
{-# Language NamedFieldPuns #-}
{-# Language ViewPatterns #-}

module AlloyDiagrams (render) where

import Datatype (UMLStateDiagram, StateDiagram(..), Connection(..), HistoryType(..), globalise)

import Data.String.Interpolate (i)
import Data.List (intercalate)
import Data.List.Extra (nubOrd)
import Data.Maybe (fromJust, catMaybes)

data Inherited = Inherited
  { ctxt :: [Int]
  , nameMapping :: [(String,String)]
  , connectionFroms :: [String]
  }

data Synthesized = Synthesized
  { alloy :: String
  , names :: [String]
  , rootNodes :: [String]
  , innerStarts :: [Maybe String]
  , endNodes :: Bool
  , normalStates :: Bool
  , hierarchicalStates :: Bool
  , regionsStates :: Bool
  , deepHistoryNodes :: Bool
  , shallowHistoryNodes :: Bool
  , forkNodes :: Bool
  , joinNodes :: Bool
  }

render :: UMLStateDiagram -> String
render (globalise -> StateDiagram{ substate, label, name, connection, startState }) =
  let Synthesized {alloy, names, innerStarts, endNodes, normalStates, hierarchicalStates, regionsStates, deepHistoryNodes, shallowHistoryNodes, forkNodes, joinNodes} =
        renderInner renderNode substate
          Inherited {ctxt = [], nameMapping = nameMapping
                    , connectionFroms = map (\Connection{ pointFrom } -> [i|N_#{address pointFrom}|]) connection}
      nameMapping = zipWith (\name -> (name,) . ("Name" ++) . show) (nubOrd names) [1..]
      nameOutput = map (\(_,component) -> [i|one sig #{component} extends ComponentNames{}|])
                   nameMapping
      transitionMapping = zipWith (\name -> (name,) . ("T" ++) . show) (nubOrd (filter (not . null) (map transition connection))) [1..]
      transitionOutput = map (\(_,trigger) -> [i|one sig #{trigger} extends TriggerNames{}|])
                         transitionMapping
      theInnerStarts = catMaybes innerStarts
      theFlows =
        [ "SFlow" | not (null startState) ]
        ++ map (++"Flow") theInnerStarts
        ++ zipWith (const $ ("Connection"++) . show) connection [1..]
      nullScopes =
        concatMap (("0 "++) . (++", ") . snd) . filter fst $
        [ (not endNodes, "EndNodes")
        , (null startState && null theInnerStarts, "StartNodes")
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
  [i|module diagram // name: #{show name}, (irrelevant) label: #{show label}
open uml_state_diagram
#{if null startState then "" else renderStart ("S", startState)}
#{alloy}
#{unlines $ zipWith (renderConnection transitionMapping) connection [1..]}
#{unlines nameOutput}
#{unlines transitionOutput}
fact{
  // #{if null theFlows then "no Flows" else "Flows = " ++ intercalate " + " theFlows}
  // #{if endNodes then "some EndNodes" else "no EndNodes"}
  // #{if null startState && null theInnerStarts then "no StartNodes" else "some StartNodes"}
  // #{if null names then "no ComponentNames" else "some ComponentNames"}
  // #{if null transitionMapping then "no TriggerNames" else "some TriggerNames"}
  // #{if normalStates then "some NormalStates" else "no NormalStates"}
  // #{if hierarchicalStates then "some HierarchicalStates" else "no HierarchicalStates"}
  // #{if regionsStates then "some RegionsStates" else "no RegionsStates"}
  // #{if deepHistoryNodes then "some DeepHistoryNodes" else "no DeepHistoryNodes"}
  // #{if shallowHistoryNodes then "some ShallowHistoryNodes" else "no ShallowHistoryNodes"}
  // #{if forkNodes then "some ForkNodes" else "no ForkNodes"}
  // #{if joinNodes then "some JoinNodes" else "no JoinNodes"}
}
run {} for #{nullScopes}#{show label} ProtoFlows, exactly #{show (length theFlows)} Flows // concerning ProtoFlows, a temporary hack for manual scope setting
|]

renderStart :: (String, [Int]) -> String
renderStart (start, target) = [i|one sig #{start} extends StartNodes{}
one sig #{start}Flow extends Flows{}{
  from = #{start}
  label = EmptyTrigger
  to = N_#{address target}
}|]

renderConnection :: [(String, String)] -> Connection -> Int -> String
renderConnection transitionMapping Connection{ pointFrom, pointTo, transition } n = [i|one sig Connection#{n} extends Flows{}{
  from = N_#{address pointFrom}
  label = #{if null transition then "EmptyTrigger" else fromJust (lookup transition transitionMapping) ++ " // " ++ show transition}
  to = N_#{address pointTo}
}|]

renderInner recurse substate inherited =
  let
    recursively = map (`recurse` inherited) substate
  in
      Synthesized
      { alloy = unlines $ map alloy recursively
      , names = concatMap names recursively
      , rootNodes = concatMap rootNodes recursively
      , innerStarts = concatMap innerStarts recursively
      , endNodes = any endNodes recursively
      , normalStates = any normalStates recursively
      , hierarchicalStates = any hierarchicalStates recursively
      , regionsStates = any regionsStates recursively
      , deepHistoryNodes = any deepHistoryNodes recursively
      , shallowHistoryNodes = any shallowHistoryNodes recursively
      , forkNodes = any forkNodes recursively
      , joinNodes = any joinNodes recursively
      }

renderComposite kind eachWith StateDiagram{ substate, label, name, startState } inh@Inherited{ctxt, nameMapping} =
  let
    here = ctxt ++ [label]
    node = [i|#{if kind == "Regions" then "R" else "N"}_#{address here}|]
    start = if null startState then Nothing else Just ([i|S_#{address here}|], here ++ startState)
    Synthesized {alloy, names, rootNodes, innerStarts, endNodes, normalStates, hierarchicalStates, regionsStates, deepHistoryNodes, shallowHistoryNodes, forkNodes, joinNodes} =
      renderInner eachWith substate
        inh {ctxt = here}
  in
  Synthesized
  { alloy = unlines $
            [i|one sig #{node} extends #{kind}{}{#{if kind == "RegionsStates" then "" else if null name then "\n  no name" else "\n  name = " ++ fromJust (lookup name nameMapping) ++ " // " ++ show name}
  contains = #{intercalate " + " (maybe [] (\x -> [fst x]) start ++ rootNodes)}
}|]
            : maybe id ((:) . renderStart) start
            [ alloy ]
  , names = if null name then names else name : names
  , rootNodes = [node]
  , innerStarts = fmap fst start : innerStarts
  , endNodes = endNodes
  , normalStates = normalStates
  , hierarchicalStates = kind == "HierarchicalStates" || hierarchicalStates
  , regionsStates = kind == "RegionsStates" || regionsStates
  , deepHistoryNodes = deepHistoryNodes
  , shallowHistoryNodes = shallowHistoryNodes
  , forkNodes = forkNodes
  , joinNodes = joinNodes
  }

defaultSynthesized :: Synthesized
defaultSynthesized = Synthesized
  { names = []
  , innerStarts = []
  , endNodes = False
  , normalStates = False
  , hierarchicalStates = False
  , regionsStates = False
  , deepHistoryNodes = False
  , shallowHistoryNodes = False
  , forkNodes = False
  , joinNodes = False
  }

renderNode :: StateDiagram a -> Inherited -> Synthesized

renderNode d@StateDiagram{} inherited =
  renderComposite "HierarchicalStates" renderNode d inherited

renderNode CombineDiagram { substate, label } inherited =
  renderComposite "RegionsStates"
    (renderComposite "Regions" renderNode)
    StateDiagram{ substate = substate, label = label, name = "", startState = [] }
    inherited

renderNode InnerMostState { label, name } Inherited{ctxt, nameMapping} =
  let
    here = ctxt ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends NormalStates{}{
  #{if null name then "no name" else "name = " ++ fromJust (lookup name nameMapping) ++ " // " ++ show name}
}|]
  , names = if null name then [] else [name]
  , rootNodes = [node]
  , normalStates = True
  }

renderNode EndState { label } Inherited{ctxt} =
  let
    here = ctxt ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends EndNodes{}|]
  , rootNodes = [node]
  , endNodes = True
  }

renderNode History { label, historyType } Inherited{ctxt} =
  let
    here = ctxt ++ [label]
    node = [i|N_#{address here}|]
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends #{show historyType}HistoryNodes{}|]
  , rootNodes = [node]
  , deepHistoryNodes = historyType == Deep
  , shallowHistoryNodes = historyType == Shallow
  }

renderNode Joint { label } Inherited{ctxt, connectionFroms} =
  let
    here = ctxt ++ [label]
    node = [i|N_#{address here}|]
    isFork = length (filter (node==) connectionFroms) > 1
  in
  defaultSynthesized
  { alloy = [i|one sig #{node} extends #{if isFork then "Fork" else "Join"}Nodes{}|]
  , rootNodes = [node]
  , forkNodes = isFork
  , joinNodes = not isFork
  }

address :: [Int] -> String
address = intercalate "_" . map show
