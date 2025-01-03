{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.StateDiagram.Config(defaultSDConfig
                                    ,checkSDConfig
                                    ,sdConfigToAlloy
                                    ,SDConfig(..)
                                    ,ChartLimits(..)
                                    ,defaultSDConfigScenario1
                                    ,defaultSDConfigScenario2
                                    ,defaultSDConfigScenario3
                                    )

where

import Modelling.StateDiagram.Alloy(componentsSigRules
                                   ,endstateRules
                                   ,historyRules
                                   ,nameRules
                                   ,nodeRules
                                   ,reachabilityRules
                                   ,regionRules
                                   ,startstateRules
                                   ,substateRules
                                   ,transitionRules
                                   ,trueReachability)
import Data.String.Interpolate(i)
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Control.Applicative (Alternative ((<|>)))

data ChartLimits
  = ChartLimits { regionsStates :: Int
                , hierarchicalStates :: Int
                , regions :: Int
                , normalStates :: Int
                , componentNames :: Int
                , triggerNames :: (Int,Int)
                , startNodes :: (Int,Int)
                , endNodes :: Int
                , forkNodes :: (Int,Int)
                , joinNodes :: (Int,Int)
                , shallowHistoryNodes :: (Int,Int)
                , deepHistoryNodes :: (Int,Int)
                , flows :: Int
                , derivedFlowsNew :: (Int,Int)
                , derivedFlowsReused :: (Int,Int)
                , totalNodes :: (Int,Int)
                }
  deriving (Show,Eq)

data SDConfig
  = SDConfig { bitwidth :: Int
             , preventAnonymousNormalStates :: Bool
             , preventNormalStateNamesDuplication :: Bool
             , preventEmptyTriggersFromStates :: Bool
             , preventTriggerNamesDuplication :: Bool
             , preventNestedEndNodes :: Bool
             , multiEdgesPresentInOriginalDiagram :: Maybe Bool
             , compoundsHaveNames :: Maybe Bool
             , preventSelfLoops :: Bool
             , preventNormalAndHierarchicalStatesWithoutOutgoingEdges :: Bool
             , chartLimits :: ChartLimits
             , extraConstraint :: String
             } deriving (Show,Eq)

defaultSDConfig :: SDConfig
defaultSDConfig
  = let SDConfig { chartLimits = ChartLimits { .. }, .. }
          = defaultSDConfigScenario1
    in SDConfig { preventTriggerNamesDuplication = True
                , chartLimits = ChartLimits { componentNames = componentNames + 1
                                            , triggerNames = (normalStates + hierarchicalStates, flows - fst startNodes)
                                            , derivedFlowsNew = (1,6)
                                            , .. }
                , compoundsHaveNames = Just True
                , extraConstraint = extraConstraint ++
                  "  and mul[3,#inner] =< mul[2,#Nodes]\n\
                  \  and one (Flows <: from).hs\n\
                  \  and lone (Flows <: to).hs\n\
                  \  and hs not in (Flows <: from).(hs.contains).to\n\
                  \"
                , .. }
-- jscpd:ignore-start
defaultSDConfigScenario1 :: SDConfig
defaultSDConfigScenario1
  = SDConfig { bitwidth = 6
             , preventAnonymousNormalStates = True
             , preventNormalStateNamesDuplication = True
             , preventEmptyTriggersFromStates = True
             , preventTriggerNamesDuplication = False
             , preventNestedEndNodes = False
             , multiEdgesPresentInOriginalDiagram = Nothing
             , compoundsHaveNames = Just False
             , preventSelfLoops = False
             , preventNormalAndHierarchicalStatesWithoutOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = 0
                             , hierarchicalStates = 1
                             , regions = 0
                             , normalStates = 8
                             , componentNames = 8
                             , triggerNames = (1,10)
                             , startNodes = (1,2)
                             , endNodes = 0
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = 11
                             , derivedFlowsNew = (1,14)
                             , derivedFlowsReused = (0,11)
                             , totalNodes = (10,11)
                             }
             , extraConstraint =
               "let hs = HierarchicalStates, inner = hs + hs.contains |\n\
               \  some ((Flows <: from).hs.to & (Nodes - inner))\n\
               \  and mul[2,#inner] >= #Nodes\n\
               \"
             }
-- jscpd:ignore-end

defaultSDConfigScenario2 :: SDConfig
defaultSDConfigScenario2
  = SDConfig { bitwidth = 6
             , preventAnonymousNormalStates = True
             , preventNormalStateNamesDuplication = True
             , preventEmptyTriggersFromStates = True
             , preventTriggerNamesDuplication = False
             , preventNestedEndNodes = True
             , multiEdgesPresentInOriginalDiagram = Nothing
             , compoundsHaveNames = Just False
             , preventSelfLoops = False
             , preventNormalAndHierarchicalStatesWithoutOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = 1
                             , hierarchicalStates = 0
                             , regions = 2
                             , normalStates = 10
                             , startNodes = (1,3)
                             , endNodes = 1
                             , componentNames = 10
                             , triggerNames = (1,11)
                             , forkNodes = (0,1)
                             , joinNodes = (0,1)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = 15
                             , derivedFlowsNew = (0,15)
                             , derivedFlowsReused = (0,15)
                             , totalNodes = (14,16)
                             }
             , extraConstraint =
               "some (ForkNodes + JoinNodes)\n\
               \let inner = RegionsStates + Regions.contains |\n\
               \  some ((Flows <: from).inner.to & (NormalStates - inner))\n\
               \mul[2,#Regions.contains] >= #Nodes\n\
               \"
             }

defaultSDConfigScenario3 :: SDConfig
defaultSDConfigScenario3
  = SDConfig { bitwidth = 6
             , preventAnonymousNormalStates = True
             , preventNormalStateNamesDuplication = True
             , preventEmptyTriggersFromStates = True
             , preventTriggerNamesDuplication = False
             , preventNestedEndNodes = False
             , multiEdgesPresentInOriginalDiagram = Nothing
             , compoundsHaveNames = Just False
             , preventSelfLoops = False
             , preventNormalAndHierarchicalStatesWithoutOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = 0
                             , hierarchicalStates = 1
                             , regions = 0
                             , normalStates = 7
                             , componentNames = 7
                             , triggerNames = (2,9)
                             , startNodes = (1,2)
                             , endNodes = 0
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,1)
                             , deepHistoryNodes = (0,1)
                             , flows = 10
                             , derivedFlowsNew = (0,10)
                             , derivedFlowsReused = (0,10)
                             , totalNodes = (10,11)
                             }
             , extraConstraint =
               "one HistoryNodes\n\
               \let hs = HierarchicalStates, inner = hs + hs.contains |\n\
               \  some ((Flows <: from).hs.to & (Nodes - inner))\n\
               \  and mul[3,#inner] >= #Nodes\n\
               \"
             }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig
                       { bitwidth
                       , preventAnonymousNormalStates
                       , preventNormalStateNamesDuplication
                       , compoundsHaveNames
                       , preventNormalAndHierarchicalStatesWithoutOutgoingEdges
                       , preventTriggerNamesDuplication
                       , preventEmptyTriggersFromStates
                       , preventNestedEndNodes
                       , chartLimits = chartLimits@ChartLimits{..}
                       }
  | bitwidth < 1 = Just "bitwidth must be greater than 0"
  | regions + fst forkNodes + fst joinNodes > 0 && regionsStates < 1
  = Just "you cannot have Regions, ForkNodes or JoinNodes when you have no RegionsStates (lower bounds inconsistent)"
  | regions + snd forkNodes + snd joinNodes > 0 && regionsStates < 1
  = Just "you cannot have Regions, ForkNodes of JoinNodes when you have no RegionsStates (upper bounds inconsistent)"
  | regions < 2 * regionsStates
  = Just "each RegionsState needs at least two Regions"
  | preventNormalStateNamesDuplication && not preventAnonymousNormalStates
  = Just "you cannot prevent normal state names duplication while not preventing anonymous normal states."
  | preventNormalStateNamesDuplication && normalStates > componentNames
  = Just "Given that you want to prevent normal state names duplication, you are setting too few component names."
  | preventNormalStateNamesDuplication && compoundsHaveNames == Just True && hierarchicalStates + regions > 0 && normalStates == componentNames
  = Just "Given that you want to prevent normal state names duplication and enforce naming regions and hierarchical states, you are setting too few component names."
  | normalStates + hierarchicalStates + regions < componentNames
  = Just "You are setting too many component names, relatively to the number of entities to be potentially named."
  | compoundsHaveNames == Just False && normalStates < componentNames
  = Just "Given that you want to avoid naming regions or hierarchical states, you are setting too many component names."
  | preventNormalStateNamesDuplication && compoundsHaveNames == Just False && normalStates /= componentNames
  = Just "Given that you want to prevent normal state names duplication and avoid naming regions or hierarchical states, you are not setting the right number of component names."
  | hierarchicalStates + regions + 1 < snd startNodes
  = Just "Your upper bound for start nodes is too high, relatively to the number of compound entities (and the top-level)."
  | hierarchicalStates + regions + 1 < endNodes
  = Just "You are setting too many end nodes, relatively to the number of compound entities (and the top-level)."
  | hierarchicalStates + regions < snd shallowHistoryNodes
  = Just "Your upper bound for shallow history nodes is too high, relatively to the number of compound entities."
  | hierarchicalStates + regions < snd deepHistoryNodes
  = Just "Your upper bound for deep history nodes is too high, relatively to the number of compound entities."
  | fst totalNodes < normalStates + hierarchicalStates + regionsStates + fst startNodes + endNodes + fst forkNodes + fst joinNodes + fst shallowHistoryNodes + fst deepHistoryNodes
  = Just "The minimum total number for Nodes is too small, compared to the other numbers (lower bounds inconsistent)."
  | snd totalNodes > normalStates + hierarchicalStates + regionsStates + snd startNodes + endNodes + snd forkNodes + snd joinNodes + snd shallowHistoryNodes + snd deepHistoryNodes
  = Just "The maximum total number for Nodes is too big, compared to the other numbers (upper bounds inconsistent)."
  | flows < fst startNodes + 2 * fst forkNodes + fst joinNodes + if preventNormalAndHierarchicalStatesWithoutOutgoingEdges then normalStates + hierarchicalStates else 0
  = Just "Your number of flows is too low, relatively to the number of nodes that will definitely have leaving flows according to your settings."
  | flows < fst startNodes + snd triggerNames + if preventEmptyTriggersFromStates then fst joinNodes else 0
  = Just "Your upper bound for trigger names is too high, relatively to the number of possibly named flows."
  | preventTriggerNamesDuplication && preventNormalAndHierarchicalStatesWithoutOutgoingEdges &&
    fst triggerNames < hierarchicalStates + if preventEmptyTriggersFromStates then normalStates - (if snd joinNodes > 0 then regions - regionsStates else 0) else 0
  = Just "Your lower bound for trigger names is too low, relatively to the number of states to have distinctly named leaving flows according to your settings."
  | preventTriggerNamesDuplication && preventEmptyTriggersFromStates &&
    fst triggerNames <
    flows - snd startNodes - snd shallowHistoryNodes - snd deepHistoryNodes
    - snd joinNodes
    - (if snd joinNodes > 0 then regions - regionsStates else 0)
    - (regions - 2 * (regionsStates - 1)) * snd forkNodes
  = Just "Your lower bound for trigger names is too low, relatively to the number of flows to be distinctly named according to your settings."
  | snd derivedFlowsReused > flows
  = Just "You cannot reuse more flows than there are."
  | maximum ([ flows + snd derivedFlowsNew | uncurry (/=) derivedFlowsNew ]
             ++ [ snd derivedFlowsReused | derivedFlowsReused /= (0, flows) ]
             ++ [ b | (a,b) <- [ startNodes
                               , shallowHistoryNodes
                               , deepHistoryNodes
                               , triggerNames
                               , forkNodes
                               , joinNodes
                               , totalNodes ]
                    , 0 < a && a /= b ])
    > 2 ^ (bitwidth - 1) - 1
  = Just "Your bitwidth setting is too small, given the other settings."
  | preventNestedEndNodes && endNodes == 0
  = Just "Setting preventNestedEndNodes is meaningless if there anyway are no EndNodes."
  | otherwise
  = checkLimits chartLimits

checkLimits :: ChartLimits -> Maybe String
checkLimits ChartLimits{..}
  = checkSingle regionsStates "RegionsStates"
    <|> checkSingle hierarchicalStates "HierarchicalStates"
    <|> checkSingle regions "Regions"
    <|> checkSingle normalStates "NormalStates"
    <|> checkPair startNodes "StartNodes"
    <|> checkSingle endNodes "EndNodes"
    <|> checkPair forkNodes "ForkNodes"
    <|> checkPair joinNodes "JoinNodes"
    <|> checkPair shallowHistoryNodes "ShallowHistoryNodes"
    <|> checkPair deepHistoryNodes "DeepHistoryNodes"
    <|> checkSingle flows "Flows"
    <|> checkPair derivedFlowsNew "derivedFlowsNew (ProtoFlows - Flows)"
    <|> checkPair derivedFlowsReused "derivedFlowsReused"
    <|> checkPair totalNodes "Nodes"
    <|> checkSingle componentNames "ComponentNames"
    <|> checkPair triggerNames "TriggerNames"
  where
    checkSingle element name
      | element < 0 = Just $ name ++ " must be greater than or equal to 0"
      | otherwise = Nothing
    checkPair (lower, upper) name
      | lower > upper = Just $ "minimum of " ++ name ++ " must be less than or equal to maximum of " ++ name
      | otherwise = checkSingle lower name

sdConfigToAlloy :: SDConfig -> String
sdConfigToAlloy  SDConfig { bitwidth
                          , preventEmptyTriggersFromStates
                          , preventTriggerNamesDuplication
                          , preventAnonymousNormalStates
                          , preventNormalStateNamesDuplication
                          , preventNestedEndNodes
                          , multiEdgesPresentInOriginalDiagram
                          , compoundsHaveNames
                          , preventSelfLoops
                          , preventNormalAndHierarchicalStatesWithoutOutgoingEdges
                          , chartLimits = ChartLimits { regionsStates
                                                      , hierarchicalStates
                                                      , regions
                                                      , normalStates
                                                      , componentNames
                                                      , triggerNames
                                                      , startNodes
                                                      , endNodes
                                                      , forkNodes
                                                      , joinNodes
                                                      , shallowHistoryNodes
                                                      , deepHistoryNodes
                                                      , flows
                                                      , derivedFlowsNew
                                                      , derivedFlowsReused
                                                      , totalNodes
                                                      }
                          , extraConstraint
                          }
  = let protoFlows = bimap (flows +) (flows +) derivedFlowsNew
    in
    [i|module GenUMLStateDiagram
      #{componentsSigRules}
      #{trueReachability}
      #{if snd startNodes > 0 then startstateRules else ""}
      #{if endNodes > 0 then endstateRules else ""}
      #{if regionsStates > 0 then regionRules else ""}
      #{if snd forkNodes + snd joinNodes > 0 then nodeRules else ""}
      #{reachabilityRules}
      #{if snd shallowHistoryNodes + snd deepHistoryNodes > 0 then historyRules else ""}
      #{transitionRules}
      #{substateRules}
      #{nameRules}

// This predicate is automatically generated from the configuration settings within Haskell.
// Please consider that setting a non-exact value to a parameter can cause very slow Alloy execution times.
pred scenarioConfig #{oB}
  #{if preventAnonymousNormalStates then "no s : NormalStates | no s.name" else ""}
  #{maybe "" (\p -> if p
                    then "(no c : HierarchicalStates | no c.name) and (no c : Regions | no c.name)"
                    else "no HierarchicalStates.name and no Regions.name")
   compoundsHaveNames}
  #{if preventNormalStateNamesDuplication then "no disj s1,s2 : NormalStates | s1.name = s2.name" else ""}
  #{if preventTriggerNamesDuplication then "all disj f1,f2 : label.TriggerNames | f1.label = f2.label implies (one (f1.to & f2.to & JoinNodes) or one (f1.from & f2.from & ForkNodes))" else ""}
  #{if preventEmptyTriggersFromStates then "all f : from.States | f.label = EmptyTrigger implies f.to in (ForkNodes + JoinNodes) & label.TriggerNames.from" else ""}
  #{maybe "" (\p
                -> (if not p then id else \c -> "not (" ++ c ++ ")")
                   "all n1, n2 : Nodes | lone (Flows & from.n1 & to.n2)")
   multiEdgesPresentInOriginalDiagram}
  #{if preventNestedEndNodes then "disj[EndNodes, allContainedNodes]" else ""}
  #{if preventSelfLoops then "no s : States | s in (Flows <: from).s.to" else ""}
  #{if preventNormalAndHierarchicalStatesWithoutOutgoingEdges then "all s : (NormalStates + HierarchicalStates) | some (Flows <: from).s" else ""}
  #{lowerBound startNodes "StartNodes"}
  #{lowerBound shallowHistoryNodes "ShallowHistoryNodes"}
  #{lowerBound deepHistoryNodes "DeepHistoryNodes"}
  #{lowerBound triggerNames "TriggerNames"}
  #{lowerBound forkNodes "ForkNodes"}
  #{lowerBound joinNodes "JoinNodes"}
  #{lowerBound protoFlows "ProtoFlows"}
  #{lowerBound totalNodes "Nodes"}
  #{derivedFlowsReusedBounds}
#{extraConstraint}
#{cB}

run scenarioConfig for #{bitwidth} Int,
#{upperBoundOrExact startNodes "StartNodes"},
exactly #{endNodes} EndNodes,
exactly #{normalStates} NormalStates,
#{upperBoundOrExact shallowHistoryNodes "ShallowHistoryNodes"},
#{upperBoundOrExact deepHistoryNodes "DeepHistoryNodes"},
exactly #{hierarchicalStates} HierarchicalStates,
exactly #{flows} Flows,
#{upperBoundOrExact protoFlows "ProtoFlows"},
exactly #{componentNames} ComponentNames,
#{upperBoundOrExact triggerNames "TriggerNames"},
exactly #{regionsStates} RegionsStates,
#{upperBoundOrExact forkNodes "ForkNodes"},
#{upperBoundOrExact joinNodes "JoinNodes"},
exactly #{regions} Regions,
#{upperBoundOrExact totalNodes "Nodes"}
    |]
  where
  oB = "{"
  cB = "}"
  lowerBound (a,b) entry
                  | 0 < a && a /= b = "#" ++ entry ++ " >= " ++ show a
                  | otherwise = ""
  upperBoundOrExact (0,0) entry
                    = "0 " ++ entry
  upperBoundOrExact (a,b) entry
                    | a == b = "exactly " ++ show a ++ " " ++ entry
                    | otherwise = show b ++ " " ++ entry
  derivedFlowsReusedBounds
    | derivedFlowsReused == (0, flows)
    = ""
    | otherwise
    = let (low, high) = derivedFlowsReused
      in
        "let reused = ProtoFlows.derived & Flows | " ++
        if low == high
        then
          "#reused = " ++ show low
        else
          intercalate " and " $
          [ "#reused >= " ++ show low | low > 0 ]
          ++
          [ "#reused =< " ++ show high | high < flows ]
