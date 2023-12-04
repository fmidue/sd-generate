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
                , protoFlows :: (Int,Int)
                , totalNodes :: (Int,Int)
                }
  deriving (Show)

data SDConfig
  = SDConfig { bitwidth :: Int
             , enforceNormalStateNames :: Bool
             , distinctNormalStateNames :: Bool
             , preventEmptyTriggersFromStates :: Bool
             , distinctTriggerNames :: Bool
             , preventNestedEndNodes :: Bool
             , preventMultiEdges :: Maybe Bool
             , compoundsHaveNames :: Maybe Bool
             , enforceOutgoingEdgesFromNormalAndHierarchical :: Bool
             , chartLimits :: ChartLimits
             , extraConstraint :: String
             } deriving (Show)

defaultSDConfig :: SDConfig
defaultSDConfig
  = let SDConfig { .. } = defaultSDConfigScenario1
    in SDConfig { distinctTriggerNames = True, .. }

defaultSDConfigScenario1 :: SDConfig
defaultSDConfigScenario1
  = SDConfig { bitwidth = 6
             , enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , preventEmptyTriggersFromStates = True
             , distinctTriggerNames = False
             , preventNestedEndNodes = False
             , preventMultiEdges = Nothing
             , compoundsHaveNames = Just False
             , enforceOutgoingEdgesFromNormalAndHierarchical = True
             , chartLimits =
                 ChartLimits { regionsStates = 0
                             , hierarchicalStates = 1
                             , regions = 0
                             , normalStates = 8
                             , componentNames = 8
                             , triggerNames = (1,10)
                             , startNodes = (0,2)
                             , endNodes = 0
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = 10
                             , protoFlows = (10,20)
                             , totalNodes = (9,11)
                             }
             , extraConstraint =
               "let hs = HierarchicalStates, inner = hs + hs.contains |\n\
               \  some ((Flows <: from).hs.to & (Nodes - inner))\n\
               \  and mul[2,#inner] >= #Nodes\n\
               \"
             }

defaultSDConfigScenario2 :: SDConfig
defaultSDConfigScenario2
  = SDConfig { bitwidth = 6
             , enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , preventEmptyTriggersFromStates = True
             , distinctTriggerNames = False
             , preventNestedEndNodes = True
             , preventMultiEdges = Nothing
             , compoundsHaveNames = Just False
             , enforceOutgoingEdgesFromNormalAndHierarchical = True
             , chartLimits =
                 ChartLimits { regionsStates = 1
                             , hierarchicalStates = 0
                             , regions = 2
                             , normalStates = 10
                             , startNodes = (0,3)
                             , endNodes = 1
                             , componentNames = 10
                             , triggerNames = (1,11)
                             , forkNodes = (0,1)
                             , joinNodes = (0,1)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = 12
                             , protoFlows = (12,24)
                             , totalNodes = (13,16)
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
             , enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , preventEmptyTriggersFromStates = True
             , distinctTriggerNames = False
             , preventNestedEndNodes = False
             , preventMultiEdges = Nothing
             , compoundsHaveNames = Just False
             , enforceOutgoingEdgesFromNormalAndHierarchical = True
             , chartLimits =
                 ChartLimits { regionsStates = 0
                             , hierarchicalStates = 1
                             , regions = 0
                             , normalStates = 7
                             , componentNames = 7
                             , triggerNames = (2,9)
                             , startNodes = (0,2)
                             , endNodes = 0
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,1)
                             , deepHistoryNodes = (0,1)
                             , flows = 9
                             , protoFlows = (9,18)
                             , totalNodes = (9,11)
                             }
             , extraConstraint =
               "one HistoryNodes\n\
               \let hs = HierarchicalStates, inner = hs + hs.contains |\n\
               \  some ((Flows <: from).hs.to & (Nodes - inner))\n\
               \  and mul[3,#inner] >= #Nodes\n\
               \"
             }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig { bitwidth
                       , enforceNormalStateNames
                       , distinctNormalStateNames
                       , chartLimits = chartLimits@ChartLimits{..}
                       }
  | bitwidth < 1 = Just "bitwidth must be greater than 0"
  | regions + fst forkNodes + fst joinNodes > 0 && regionsStates < 1
  = Just "you cannot have Regions, ForkNodes or JoinNodes when you have no RegionsStates (lower bounds inconsistent)"
  | regions + snd forkNodes + snd joinNodes > 0 && regionsStates < 1
  = Just "you cannot have Regions, ForkNodes of JoinNodes when you have no RegionsStates (upper bounds inconsistent)"
  | regions < 2 * regionsStates
  = Just "each RegionsState needs at least two Regions"
  | distinctNormalStateNames && not enforceNormalStateNames = Just "you cannot enforce distinct normal state names without enforcing normal state names"
  | distinctNormalStateNames && normalStates > componentNames
  = Just "Given that you want to enforce distinct normal state names, you are setting too few component names."
  | fst totalNodes < normalStates + hierarchicalStates + regionsStates + fst startNodes + endNodes + fst forkNodes + fst joinNodes + fst shallowHistoryNodes + fst deepHistoryNodes
  = Just "The minimum total number for Nodes is too small, compared to the other numbers (lower bounds inconsistent)."
  | snd totalNodes > normalStates + hierarchicalStates + regionsStates + snd startNodes + endNodes + snd forkNodes + snd joinNodes + snd shallowHistoryNodes + snd deepHistoryNodes
  = Just "The maximum total number for Nodes is too big, compared to the other numbers (upper bounds inconsistent)."
  | otherwise
  = checkLimits chartLimits <|> checkAmounts chartLimits

checkLimits :: ChartLimits -> Maybe String
checkLimits ChartLimits{..}
  = checkPair (regionsStates, regionsStates) "RegionsStates"
    <|> checkPair (hierarchicalStates, hierarchicalStates) "HierarchicalStates"
    <|> checkPair (regions, regions) "Regions"
    <|> checkPair (normalStates, normalStates) "NormalStates"
    <|> checkPair startNodes "StartNodes"
    <|> checkPair (endNodes, endNodes) "EndNodes"
    <|> checkPair forkNodes "ForkNodes"
    <|> checkPair joinNodes "JoinNodes"
    <|> checkPair shallowHistoryNodes "ShallowHistoryNodes"
    <|> checkPair deepHistoryNodes "DeepHistoryNodes"
    <|> checkPair (flows, flows) "Flows"
    <|> checkPair protoFlows "ProtoFlows"
    <|> checkPair totalNodes "Nodes"
    <|> checkPair (componentNames, componentNames) "ComponentNames"
    <|> checkPair triggerNames "TriggerNames"
  where
    checkPair pair name
      | uncurry (>) pair = Just $ "minimum of " ++ name ++ " must be less than or equal to maximum of " ++ name
      | fst pair < 0 = Just $ name ++ " must be greater than or equal to 0"
      | otherwise = Nothing

checkAmounts :: ChartLimits -> Maybe String
checkAmounts ChartLimits{..}
  = sumNotExceededBy [(flows, flows)] triggerNames "trigger names, relatively to the number of flows"
    <|> sumNotExceededBy [(normalStates, normalStates), (hierarchicalStates, hierarchicalStates), (regions, regions)] (componentNames, componentNames) "component names, relatively to entities to be potentially named"
    <|> sumNotExceededBy [(hierarchicalStates, hierarchicalStates), (regions, regions), (1,1)] startNodes "start nodes, relatively to compound entities"
    <|> sumNotExceededBy [(hierarchicalStates, hierarchicalStates), (regions, regions), (1,1)] (endNodes, endNodes) "end nodes, relatively to compound entities"
    <|> sumNotExceededBy [(hierarchicalStates, hierarchicalStates), (regions, regions)] shallowHistoryNodes "shallow history nodes, relatively to compound entities"
    <|> sumNotExceededBy [(hierarchicalStates, hierarchicalStates), (regions, regions)] deepHistoryNodes "deep history nodes, relatively to compound entities"
    <|> sumNotExceededBy [protoFlows] (flows, flows) "flows, relatively to the proto flows"
  where
    sumNotExceededBy these that ofIt
      | sum (map fst these) < fst that
      = Just $ "You seem to be requesting too many " ++ ofIt ++ " (lower bounds inconsistent)."
      | sum (map snd these) < snd that
      = Just $ "You seem to be requesting too many " ++ ofIt ++ " (upper bounds inconsistent)."
      | otherwise = Nothing

sdConfigToAlloy :: SDConfig -> String
sdConfigToAlloy  SDConfig { bitwidth
                          , preventEmptyTriggersFromStates
                          , distinctTriggerNames
                          , enforceNormalStateNames
                          , distinctNormalStateNames
                          , preventNestedEndNodes
                          , preventMultiEdges
                          , compoundsHaveNames
                          , enforceOutgoingEdgesFromNormalAndHierarchical
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
                                                      , protoFlows
                                                      , totalNodes
                                                      }
                          , extraConstraint
                          }
  = [i|module GenUMLStateDiagram
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
  #{if enforceNormalStateNames then "no s : NormalStates | no s.name" else ""}
  #{maybe "" (\p -> if p
                    then "no c : (HierarchicalStates + Regions) | no c.name"
                    else "ComponentNames in NormalStates.name")
   compoundsHaveNames}
  #{if distinctNormalStateNames then "no disj s1,s2 : NormalStates | s1.name = s2.name" else ""}
  #{if distinctTriggerNames then "no disj f1,f2 : label.TriggerNames | f1.label = f2.label" else ""}
  #{if preventEmptyTriggersFromStates then "EmptyTrigger not in from.States.label" else ""}
  #{maybe "" (\p
                -> (if p then id else \c -> "not (" ++ c ++ ")")
                   "all n1, n2 : Nodes | lone (Flows & from.n1 & to.n2)")
   preventMultiEdges}
  #{if preventNestedEndNodes then "disj[EndNodes, allContainedNodes]" else ""}
  #{if enforceOutgoingEdgesFromNormalAndHierarchical then "all s : (NormalStates + HierarchicalStates) | some (Flows <: from).s" else ""}
  #{bounded (regions, regions) "Regions"}
  #{bounded (regionsStates, regionsStates) "RegionsStates"}
  #{bounded (hierarchicalStates, hierarchicalStates) "HierarchicalStates"}
  #{bounded startNodes "StartNodes"}
  #{bounded (endNodes, endNodes) "EndNodes"}
  #{bounded shallowHistoryNodes "ShallowHistoryNodes"}
  #{bounded deepHistoryNodes "DeepHistoryNodes"}
  #{bounded (normalStates, normalStates) "NormalStates"}
  #{bounded (componentNames, componentNames) "ComponentNames"}
  #{bounded triggerNames "TriggerNames"}
  #{bounded forkNodes "ForkNodes"}
  #{bounded joinNodes "JoinNodes"}
  #{bounded (flows, flows) "Flows"}
  #{bounded protoFlows "ProtoFlows"}
  #{bounded totalNodes "Nodes"}
#{extraConstraint}
#{cB}

run scenarioConfig for #{bitwidth} Int,
#{caseExact startNodes "StartNodes"},
#{caseExact (endNodes, endNodes) "EndNodes"},
#{caseExact (normalStates, normalStates) "NormalStates"},
#{caseExact shallowHistoryNodes "ShallowHistoryNodes"},
#{caseExact deepHistoryNodes "DeepHistoryNodes"},
#{caseExact (hierarchicalStates, hierarchicalStates) "HierarchicalStates"},
#{caseExact (flows, flows) "Flows"},
#{caseExact protoFlows "ProtoFlows"},
#{caseExact (componentNames, componentNames) "ComponentNames"},
#{caseExact triggerNames "TriggerNames"},
#{caseExact (regionsStates, regionsStates) "RegionsStates"},
#{caseExact forkNodes "ForkNodes"},
#{caseExact joinNodes "JoinNodes"},
#{caseExact (regions, regions) "Regions"},
#{caseExact totalNodes "Nodes"}
    |]
  where
  oB = "{"
  cB = "}"
  bounded (a,b) entry
                  | 0 < a && a /= b = "#" ++ entry ++ " >= " ++ show a
                  | otherwise = ""
  caseExact (0,0) entry
                    = "0 " ++ entry
  caseExact (a,b) entry
                    | a == b = "exactly " ++ show a ++ " " ++ entry
                    | otherwise = show b ++ " " ++ entry
