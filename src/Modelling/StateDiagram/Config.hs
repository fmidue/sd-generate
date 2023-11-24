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
                , endNodes :: (Int,Int)
                , forkNodes :: (Int,Int)
                , joinNodes :: (Int,Int)
                , historyNodes :: (Int,Int)
                }
  deriving (Show)

scenario1ChartConfig :: ChartConfig
scenario1ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (0,0)
                , normalStates = (7,7)
                , endNodes = (0,0)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (0,0)
                }

scenario2ChartConfig :: ChartConfig
scenario2ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (2,2)
                , normalStates = (8,8)
                , endNodes = (1,1)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (0,0)
                }

scenario3ChartConfig :: ChartConfig
scenario3ChartConfig
  = ChartConfig { regionStates = (0,0)
                , hierarchicalStates = (1,1)
                , regions = (0,0)
                , normalStates = (8,8)
                , endNodes = (0,0)
                , forkNodes = (0,0)
                , joinNodes = (0,0)
                , historyNodes = (2,2)
                }

data SDConfig
  = SDConfig { scope :: Int
             , bitwidth :: Int
             ,  enforceNormalStateNames :: Bool
             , distinctNormalStateNames :: Bool
             , componentNames :: Int
             , flows :: Int
             , protoFlows :: Int
             , noEmptyTriggers :: Bool
             , minTotalNodes :: Int
             , noNestedEndNodes :: Bool
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
             , componentNames = 8
             , flows = 10
             , protoFlows = 3
             , noEmptyTriggers = True
             , noNestedEndNodes = False
             , minTotalNodes = 8
             , chartConfig = scenario1ChartConfig
    }

defaultSDConfigScenario2 :: SDConfig
defaultSDConfigScenario2
  = SDConfig { scope = 15
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , componentNames = 11
             , flows = 10
             , protoFlows = 0
             , noEmptyTriggers = True
             , noNestedEndNodes = True
             , minTotalNodes = 8
             , chartConfig = scenario2ChartConfig
    }

defaultSDConfigScenario3 :: SDConfig
defaultSDConfigScenario3
  = SDConfig { scope = 10
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , componentNames = 11
             , flows = 10
             , protoFlows = 0
             , noEmptyTriggers = True
             , noNestedEndNodes = False
             , minTotalNodes = 8
             , chartConfig = scenario3ChartConfig
    }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig { scope
                       , bitwidth
                       , enforceNormalStateNames
                       , distinctNormalStateNames
                       , componentNames
                       , flows
                       , protoFlows
                       , minTotalNodes
                       , chartConfig = ChartConfig { regionStates
                                                   , hierarchicalStates
                                                   , regions
                                                   , normalStates
                                                   , endNodes
                                                   , forkNodes
                                                   , joinNodes
                                                   , historyNodes
                                                   }
                       }
  | scope < 1 = Just "scope must be greater than 0"
  | bitwidth < 1 = Just "bitwidth must be greater than 0"
  | uncurry (>) regionStates = Just "minRegionStates must be less than or equal to maxRegionStates"
  | uncurry (>) hierarchicalStates = Just "minHierarchicalStates must be less than or equal to maxHierarchicalStates"
  | uncurry (>) regions = Just "minRegions must be less than or equal to maxRegions"
  | uncurry (>) normalStates = Just "minNormalStates must be less than or equal to maxNormalStates"
  | componentNames < 0 = Just "componentNames must be greater than or equal to 0"
  | uncurry (>) endNodes = Just "minEndNodes must be less than or equal to maxEndNodes"
  | uncurry (>) forkNodes = Just "minForkNodes must be less than or equal to maxForkNodes"
  | uncurry (>) joinNodes = Just "minJoinNodes must be less than or equal to maxJoinNodes"
  | uncurry (>) historyNodes = Just "minHistoryNodes must be less than or equal to maxHistoryNodes"
  | flows < 0 = Just "flows must be greater than or equal to 0"
  | protoFlows < 0 = Just "protoFlows must be greater than or equal to 0"
  | fst endNodes < 1 = Just "you likely want to have at least one EndNode in the chart"
  | snd regions > 6 = Just "you likely want to have less than 7 Regions in the chart"
  | snd normalStates > 15 = Just "you likely want to have less than 16 NormalStates in the chart"
  | componentNames > 15 = Just "you likely want to have less than 16 ComponentNames in the chart"
  | snd endNodes > 4 = Just "you likely want to have less than 4 EndNodes in the chart"
  | snd forkNodes > 3 = Just "you likely want to have less than 4 ForkNodes in the chart"
  | snd joinNodes > 3 = Just "you likely want to have less than 4 JoinNodes in the chart"
  | snd historyNodes > 5 = Just "you likely want to have less than 6 HistoryNodes in the chart"
  | distinctNormalStateNames && not enforceNormalStateNames = Just "you cannot enforce distinct normal state names without enforcing normal state names"
  | minTotalNodes < 4 = Just "you likely want to have at least 4 Nodes in the chart"
  | otherwise = Nothing

sdConfigToAlloy :: SDConfig -> String
sdConfigToAlloy  SDConfig { scope
                          , bitwidth
                          , componentNames
                          , flows
                          , protoFlows
                          , minTotalNodes
                          , noEmptyTriggers
                          , enforceNormalStateNames
                          , distinctNormalStateNames
                          , noNestedEndNodes
                          , chartConfig = ChartConfig { regionStates
                                                      , hierarchicalStates
                                                      , regions
                                                      , normalStates
                                                      , endNodes
                                                      , forkNodes
                                                      , joinNodes
                                                      , historyNodes
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

// this predicate is automatically generated from the configuration settings within Haskell
pred scenarioConfig #{oB}
  #{if enforceNormalStateNames then "no s : NormalStates | no s.name" else ""}
  #{if distinctNormalStateNames then "no disjoint s1,s2 : NormalStates | s1.name = s2.name" else ""}
  #{if noEmptyTriggers then "EmptyTrigger not in from.States.label" else ""}
  #{if snd joinNodes >= 1 && snd forkNodes >= 1 then "some (ForkNodes + JoinNodes)" else ""}
  #{if noNestedEndNodes then "EndNodes not in allContainedNodes" else ""}
  #{"#Nodes >= " ++ show minTotalNodes}
#{cB}

run scenarioConfig for #{scope} but #{bitwidth} Int, #{snd regionStates} RegionsStates, exactly #{snd hierarchicalStates} HierarchicalStates,
#{snd regions} Regions, exactly #{snd normalStates} NormalStates, #{componentNames} ComponentNames, #{snd endNodes} EndNodes,
#{snd forkNodes} ForkNodes, #{snd joinNodes} JoinNodes, #{snd historyNodes} HistoryNodes, exactly #{flows} Flows, #{protoFlows} ProtoFlows
    |]
  where
  oB = "{" -- escaping issues
  cB = "}"
