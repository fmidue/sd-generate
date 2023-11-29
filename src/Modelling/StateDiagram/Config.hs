{-# LANGUAGE NamedFieldPuns #-}
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

data ChartLimits
  = ChartLimits { regionsStates :: (Int,Int)
                , hierarchicalStates :: (Int,Int)
                , regions :: (Int,Int)
                , normalStates :: (Int,Int)
                , componentNames :: (Int,Int)
                , triggerNames :: (Int,Int)
                , startNodes :: (Int,Int)
                , endNodes :: (Int,Int)
                , forkNodes :: (Int,Int)
                , joinNodes :: (Int,Int)
                , shallowHistoryNodes :: (Int,Int)
                , deepHistoryNodes :: (Int,Int)
                , flows :: (Int,Int)
                , protoFlows :: (Int,Int)
                , totalNodes :: (Int,Int)
                }
  deriving (Show)

data SDConfig
  = SDConfig { scope :: Int
             , bitwidth :: Int
             ,  enforceNormalStateNames :: Bool
             , distinctNormalStateNames :: Bool
             , noEmptyTriggers :: Bool
             , noNestedEndNodes :: Bool
             , preventMultiEdges :: Maybe Bool
             , enforceOutgoingEdges :: Bool
             , chartLimits :: ChartLimits
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
             , preventMultiEdges = Just False
             , enforceOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = (0,0)
                             , hierarchicalStates = (1,1)
                             , regions = (0,0)
                             , normalStates = (7,7)
                             , componentNames = (0,10)
                             , triggerNames = (0,10)
                             , startNodes = (0,0)
                             , endNodes = (0,0)
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = (11,11)
                             , protoFlows = (17,17)
                             , totalNodes = (10,10)
                             }
             }

defaultSDConfigScenario2 :: SDConfig
defaultSDConfigScenario2
  = SDConfig { scope = 15
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , noEmptyTriggers = True
             , noNestedEndNodes = True
             , preventMultiEdges = Just False
             , enforceOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = (0,0)
                             , hierarchicalStates = (1,1)
                             , regions = (2,2)
                             , normalStates = (8,8)
                             , startNodes = (1,1)
                             , endNodes = (1,1)
                             , componentNames = (9,9)
                             , triggerNames = (9,9)
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (0,0)
                             , deepHistoryNodes = (0,0)
                             , flows = (10,10)
                             , protoFlows = (10,10)
                             , totalNodes = (12,12)
                             }
             }

defaultSDConfigScenario3 :: SDConfig
defaultSDConfigScenario3
  = SDConfig { scope = 10
             , bitwidth = 6
             ,  enforceNormalStateNames = True
             , distinctNormalStateNames = True
             , noEmptyTriggers = True
             , noNestedEndNodes = False
             , preventMultiEdges = Just False
             , enforceOutgoingEdges = True
             , chartLimits =
                 ChartLimits { regionsStates = (0,0)
                             , hierarchicalStates = (1,1)
                             , regions = (0,0)
                             , normalStates = (8,8)
                             , componentNames = (11,11)
                             , triggerNames = (11,11)
                             , startNodes = (0,0)
                             , endNodes = (0,0)
                             , forkNodes = (0,0)
                             , joinNodes = (0,0)
                             , shallowHistoryNodes = (1,1)
                             , deepHistoryNodes = (1,1)
                             , flows = (10,10)
                             , protoFlows = (10,10)
                             , totalNodes = (11,11)
                             }
             }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig { scope
                       , bitwidth
                       , enforceNormalStateNames
                       , distinctNormalStateNames
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
                       }
  | fst protoFlows < fst flows = Just "the minimum amount of protoFlows must at least match or better even exceed the lower bound of 'normal' flows"
  | scope < 1 = Just "scope must be greater than 0"
  | bitwidth < 1 = Just "bitwidth must be greater than 0"
  | uncurry (>) regionsStates = Just "minRegionsStates must be less than or equal to maxRegionsStates"
  | uncurry (>) hierarchicalStates = Just "minHierarchicalStates must be less than or equal to maxHierarchicalStates"
  | uncurry (>) regions = Just "minRegions must be less than or equal to maxRegions"
  | uncurry (>) normalStates = Just "minNormalStates must be less than or equal to maxNormalStates"
  | fst componentNames < 0 = Just "componentNames must be greater than or equal to 0"
  | fst triggerNames < 0 = Just "triggerNames must be greater than or equal to 0"
  | uncurry (>) startNodes = Just "minStartNodes must be less than or equal to maxStartNodes"
  | uncurry (>) endNodes = Just "minEndNodes must be less than or equal to maxEndNodes"
  | uncurry (>) forkNodes = Just "minForkNodes must be less than or equal to maxForkNodes"
  | uncurry (>) joinNodes = Just "minJoinNodes must be less than or equal to maxJoinNodes"
  | uncurry (>) shallowHistoryNodes = Just "minShallowHistoryNodes must be less than or equal to maxShallowHistoryNodes"
  | uncurry (>) deepHistoryNodes = Just "minDeepHistoryNodes must be less than or equal to maxDeepHistoryNodes"
  | uncurry (>) flows = Just "minFlows must be less than or equal to maxFlows"
  | fst protoFlows < 0 = Just "protoFlows must be greater than or equal to 0"
  | fst startNodes < 1 = Just "you likely want to have at least one startNode in the chart"
  | fst endNodes < 1 = Just "you likely want to have at least one EndNode in the chart"
  | snd regions > 6 = Just "you likely want to have less than 7 Regions in the chart"
  | snd normalStates > 15 = Just "you likely want to have less than 16 NormalStates in the chart"
  | snd componentNames > 16 = Just "you likely want to have less than 16 ComponentNames in the chart"
  | snd triggerNames > 16 = Just "you likely want to have less than 16 TriggerNames in the chart"
  | snd startNodes > 4 = Just "you likely want to have less than 4 StartNodes in the chart"
  | snd endNodes > 4 = Just "you likely want to have less than 4 EndNodes in the chart"
  | snd forkNodes > 3 = Just "you likely want to have less than 4 ForkNodes in the chart"
  | snd joinNodes > 3 = Just "you likely want to have less than 4 JoinNodes in the chart"
  | snd shallowHistoryNodes > 5 = Just "you likely want to have less than 6 ShallowHistoryNodes in the chart"
  | snd deepHistoryNodes > 5 = Just "you likely want to have less than 6 DeepHistoryNodes in the chart"
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
                          , preventMultiEdges
                          , enforceOutgoingEdges
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
  #{maybe "" (\p
                -> if p
                   then "all n1, n2 : Nodes | lone (Flows & from.n1 & to.n2)"
                   else "not (all n1, n2 : Nodes | lone (Flows & from.n1 & to.n2))")
   preventMultiEdges}
  #{if noNestedEndNodes then "EndNodes not in allContainedNodes" else ""}
  #{bounded regions "Regions"}
  #{bounded hierarchicalStates "HierarchicalStates"}
  #{bounded startNodes "StartNodes"}
  #{bounded endNodes "EndNodes"}
  #{bounded shallowHistoryNodes "ShallowHistoryNodes"}
  #{bounded deepHistoryNodes "DeepHistoryNodes"}
  #{bounded normalStates "NormalStates"}
  #{bounded componentNames "ComponentNames"}
  #{bounded triggerNames "TriggerNames"}
  #{bounded forkNodes "ForkNodes"}
  #{bounded joinNodes "JoinNodes"}
  #{bounded flows "Flows"}
  #{if uncurry (<) totalNodes
    then "#Nodes >= " ++ show (fst totalNodes) ++ " and #Nodes <= " ++ show (snd totalNodes)
    else "#Nodes = " ++ show (snd totalNodes)}
  #{if enforceOutgoingEdges
    then "all s : States | some (Flows <: from).s"
    else ""}
#{cB}

run scenarioConfig for #{scope} but #{bitwidth} Int,
#{caseExact startNodes "StartNodes"},
#{caseExact endNodes "EndNodes"},
#{caseExact normalStates "NormalStates"},
#{caseExact shallowHistoryNodes "ShallowHistoryNodes"},
#{caseExact deepHistoryNodes "DeepHistoryNodes"},
#{caseExact hierarchicalStates "HierarchicalStates"},
#{caseExact flows "Flows"},
#{show (snd protoFlows) ++ " ProtoFlows"},
#{caseExact componentNames "ComponentNames"},
#{caseExact triggerNames "TriggerNames"},
#{caseExact regionsStates "RegionsStates"},
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
