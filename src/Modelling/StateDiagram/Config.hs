{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.StateDiagram.Config(defaultSDConfig
                                    ,checkSDConfig
                                    ,sdConfigToAlloy
                                    ,SDConfig(..)
                                    ,ChartConfig(..)
                                    ,scenario1ChartConfig
                                    ,scenario2ChartConfig
                                    ,scenario3ChartConfig
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

data ChartConfig
  = ChartConfig { regionStates :: (Int,Int)
                , hierarchicalStates :: (Int,Int)
                , regions :: (Int,Int)
                , normalStates :: (Int,Int)
                , componentNames :: (Int,Int)
                , endNodes :: (Int,Int)
                , forkNodes :: (Int,Int)
                , joinNodes :: (Int,Int)
                , historyNodes :: (Int,Int)
                , flows :: (Int,Int)
                , protoFlows :: (Int,Int)
                , totalNodes :: (Int,Int)
                }
  deriving (Show)

scenario1ChartConfig :: ChartConfig
scenario1ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (0,0)
                , normalStates = (8,8)
                , componentNames = (0,10)
                , endNodes = (0,0)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (0,0)
                , flows = (10,10)
                , protoFlows = (15,15)
                , totalNodes = (10,12)
                }

scenario2ChartConfig :: ChartConfig
scenario2ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (2,2)
                , normalStates = (8,8)
                , endNodes = (1,1)
                , componentNames = (9,9)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (0,0)
                , flows = (10,10)
                , protoFlows = (10,10)
                , totalNodes = (12,12)
                }

scenario3ChartConfig :: ChartConfig
scenario3ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (0,0)
                , normalStates = (8,8)
                , componentNames = (11,11)
                , endNodes = (0,0)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (2,2)
                , flows = (10,10)
                , protoFlows = (10,10)
                , totalNodes = (11,11)
                }

data SDConfig
  = SDConfig { scope :: Int
             , bitwidth :: Int
             ,  enforceNormalStateNames :: Bool
             , distinctNormalStateNames :: Bool
             , noEmptyTriggers :: Bool
             , noNestedEndNodes :: Bool
             , ensureReachability :: Bool
             , chartConfig :: ChartConfig
             } deriving (Show)

defaultSDConfig :: SDConfig
defaultSDConfig
  = defaultSDConfigScenario1

defaultSDConfigScenario1 :: SDConfig
defaultSDConfigScenario1
  = SDConfig { scope = 10
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , noEmptyTriggers = True
             , noNestedEndNodes = False
             , ensureReachability = True
             , chartConfig = scenario1ChartConfig
    }

defaultSDConfigScenario2 :: SDConfig
defaultSDConfigScenario2
  = SDConfig { scope = 15
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , noEmptyTriggers = True
             , noNestedEndNodes = True
             , ensureReachability = True
             , chartConfig = scenario2ChartConfig
    }

defaultSDConfigScenario3 :: SDConfig
defaultSDConfigScenario3
  = SDConfig { scope = 10
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , noEmptyTriggers = True
             , noNestedEndNodes = False
             , ensureReachability = True
             , chartConfig = scenario3ChartConfig
    }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig { scope
                       , bitwidth
                       , enforceNormalStateNames
                       , distinctNormalStateNames
                       , chartConfig = ChartConfig { regionStates
                                                   , hierarchicalStates
                                                   , regions
                                                   , normalStates
                                                   , componentNames
                                                   , endNodes
                                                   , forkNodes
                                                   , joinNodes
                                                   , historyNodes
                                                   , flows
                                                   , protoFlows
                                                   , totalNodes
                                                   }
                       }
  | protoFlows < flows = Just "protoFlows must be greater than or equal to flows"
  | scope < 1 = Just "scope must be greater than 0"
  | bitwidth < 1 = Just "bitwidth must be greater than 0"
  | uncurry (>) regionStates = Just "minRegionStates must be less than or equal to maxRegionStates"
  | uncurry (>) hierarchicalStates = Just "minHierarchicalStates must be less than or equal to maxHierarchicalStates"
  | uncurry (>) regions = Just "minRegions must be less than or equal to maxRegions"
  | uncurry (>) normalStates = Just "minNormalStates must be less than or equal to maxNormalStates"
  | fst componentNames < 0 = Just "componentNames must be greater than or equal to 0"
  | uncurry (>) endNodes = Just "minEndNodes must be less than or equal to maxEndNodes"
  | uncurry (>) forkNodes = Just "minForkNodes must be less than or equal to maxForkNodes"
  | uncurry (>) joinNodes = Just "minJoinNodes must be less than or equal to maxJoinNodes"
  | uncurry (>) historyNodes = Just "minHistoryNodes must be less than or equal to maxHistoryNodes"
  | uncurry (>) flows = Just "minFlows must be less than or equal to maxFlows"
  | fst protoFlows < 0 = Just "protoFlows must be greater than or equal to 0"
  | fst endNodes < 1 = Just "you likely want to have at least one EndNode in the chart"
  | snd regions > 6 = Just "you likely want to have less than 7 Regions in the chart"
  | snd normalStates > 15 = Just "you likely want to have less than 16 NormalStates in the chart"
  | snd componentNames > 16 = Just "you likely want to have less than 16 ComponentNames in the chart"
  | snd endNodes > 4 = Just "you likely want to have less than 4 EndNodes in the chart"
  | snd forkNodes > 3 = Just "you likely want to have less than 4 ForkNodes in the chart"
  | snd joinNodes > 3 = Just "you likely want to have less than 4 JoinNodes in the chart"
  | snd historyNodes > 5 = Just "you likely want to have less than 6 HistoryNodes in the chart"
  | distinctNormalStateNames && not enforceNormalStateNames = Just "you cannot enforce distinct normal state names without enforcing normal state names"
  | uncurry (>) totalNodes = Just "you likely want to have at least 4 Nodes in the chart"
  | otherwise = Nothing

sdConfigToAlloy :: SDConfig -> String
sdConfigToAlloy  SDConfig { scope
                          , bitwidth
                          , noEmptyTriggers
                          , enforceNormalStateNames
                          , distinctNormalStateNames
                          , noNestedEndNodes
                          , ensureReachability
                          , chartConfig = ChartConfig { regionStates
                                                      , hierarchicalStates
                                                      , regions
                                                      , normalStates
                                                      , componentNames
                                                      , endNodes
                                                      , forkNodes
                                                      , joinNodes
                                                      , historyNodes
                                                      , flows
                                                      , protoFlows
                                                      , totalNodes
                                                      }
                          }
  = [i|module GenUMLStateDiagram
      #{componentsSigRules}
      #{trueReachability}
      #{startstateRules}
      #{endstateRules}
      #{regionRules}
      #{nodeRules}
      #{reachabilityRules}
      #{historyRules}
      #{transitionRules}
      #{substateRules}
      #{nameRules}

// This predicate is automatically generated from the configuration settings within Haskell.
// Please consider that setting a non-exact value to a parameter can cause very slow Alloy execution times.
pred scenarioConfig #{oB}
  #{if enforceNormalStateNames then "no s : NormalStates | no s.name" else ""}
  #{if distinctNormalStateNames then "no disj s1,s2 : NormalStates | s1.name = s2.name" else ""}
  #{if noEmptyTriggers then "EmptyTrigger not in from.States.label" else ""}
  //#{if snd joinNodes >= 1 && snd forkNodes >= 1 then "some (ForkNodes + JoinNodes)" else ""}
  #{if noNestedEndNodes then "EndNodes not in allContainedNodes" else ""}
  #{if ensureReachability then "all s : States | some (Flows <: from).s" else ""}
  #{bounded regions "Regions"}
  #{bounded hierarchicalStates "HierarchicalStates"}
  #{bounded endNodes "EndNodes"}
  #{bounded historyNodes "HistoryNodes"}
  #{bounded normalStates "NormalStates"}
  #{bounded componentNames "ComponentNames"}
  #{bounded forkNodes "ForkNodes"}
  #{bounded joinNodes "JoinNodes"}
  #{bounded flows "Flows"}
  #{if uncurry (<) totalNodes then "#Nodes >= " ++ show (fst totalNodes) ++ " and #Nodes <= " ++ show (snd totalNodes)  else "#Nodes = " ++ show (snd totalNodes)}
#{cB}

run scenarioConfig for #{scope} but #{bitwidth} Int,
#{caseExact endNodes "EndNodes"},
#{caseExact normalStates "NormalStates"},
#{caseExact historyNodes "HistoryNodes"},
#{caseExact hierarchicalStates "HierarchicalStates"},
#{caseExact flows "Flows"},
#{caseExact protoFlows "ProtoFlows"},
#{caseExact componentNames "ComponentNames"},
#{caseExact regionStates "RegionsStates"},
#{caseExact forkNodes "ForkNodes"},
#{caseExact joinNodes "JoinNodes"},
#{caseExact regions "Regions"}
    |]
  where
  oB = "{"
  cB = "}"
  bounded x entry | uncurry (==) x && fst x == 0 = "no " ++ entry
                  | uncurry (==) x && fst x == 1 = "one " ++ entry
                  | uncurry (<) x = "#" ++ entry ++ " >= " ++ show (fst x)
                  | otherwise = ""
  caseExact x entry | uncurry (==) x && fst x == 0 = "0 " ++ entry
                    | uncurry (==) x = "exactly " ++ show (fst x) ++ " " ++ entry
                    | otherwise = show (snd x) ++ " " ++ entry
