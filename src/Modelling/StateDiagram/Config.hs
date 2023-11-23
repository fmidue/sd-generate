{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.StateDiagram.Config(defaultSDConfig
                                    ,checkSDConfig
                                    ,sdConfigToAlloy
                                    ,SDConfig(..))

where

import Modelling.StateDiagram.Alloy
import Data.String.Interpolate(i)

{-
  These parameters are chosen to match the .als files definitions
  of the Alloy generator taken from the 5th release.
  The min parameter, will be used as a lower boundary to flush out
  obtained charts from the generator when they are not satisfied
-}
data SDConfig
  = SDConfig {
      minRegionStates :: Int
    , maxRegionStates :: Int
    , minHierarchicalStates :: Int
    , maxHierarchicalStates :: Int
    , minRegions :: Int
    , maxRegions :: Int
    , minNormalStates :: Int
    , maxNormalStates :: Int
    , minComponentNames :: Int
    , maxComponentNames :: Int
    , minEndNodes :: Int
    , maxEndNodes :: Int
    , minForkNodes :: Int
    , maxForkNodes :: Int
    , minJoinNodes :: Int
    , maxJoinNodes :: Int
    , minHistoryNodes :: Int
    , maxHistoryNodes :: Int
    , minFlows :: Int
    , maxFlows :: Int
    } deriving (Show)

defaultSDConfig :: SDConfig
defaultSDConfig
  = SDConfig { minRegionStates = 0
             , maxRegionStates = 0
             , minHierarchicalStates = 1
             , maxHierarchicalStates = 2 -- stalls on 1
             , minRegions = 0
             , maxRegions = 0
             , minNormalStates = 0
             , maxNormalStates = 5
             , minComponentNames = 0
             , maxComponentNames = 11
             , minEndNodes = 1
             , maxEndNodes = 1
             , minForkNodes = 0
             , maxForkNodes = 0
             , minJoinNodes = 0
             , maxJoinNodes = 0
             , minHistoryNodes = 0
             , maxHistoryNodes = 0
             , minFlows = 7
             , maxFlows = 10
             -- minStartNodes
    }

checkSDConfig :: SDConfig -> Maybe String
checkSDConfig SDConfig {
                minRegionStates
              , maxRegionStates
              , minHierarchicalStates
              , maxHierarchicalStates
              , minRegions
              , maxRegions
              , minNormalStates
              , maxNormalStates
              , minComponentNames
              , maxComponentNames
              , minEndNodes
              , maxEndNodes
              , minForkNodes
              , maxForkNodes
              , minJoinNodes
              , maxJoinNodes
              , minHistoryNodes
              , maxHistoryNodes
              , minFlows
              , maxFlows
              }
  | minRegionStates > maxRegionStates = Just "minRegionStates must be less than or equal to maxRegionStates"
  | minHierarchicalStates > maxHierarchicalStates = Just "minHierarchicalStates must be less than or equal to maxHierarchicalStates"
  | minRegions > maxRegions = Just "minRegions must be less than or equal to maxRegions"
  | minNormalStates > maxNormalStates = Just "minNormalStates must be less than or equal to maxNormalStates"
  | minComponentNames > maxComponentNames = Just "minComponentNames must be less than or equal to maxComponentNames"
  | minEndNodes > maxEndNodes = Just "minEndNodes must be less than or equal to maxEndNodes"
  | minForkNodes > maxForkNodes = Just "minForkNodes must be less than or equal to maxForkNodes"
  | minJoinNodes > maxJoinNodes = Just "minJoinNodes must be less than or equal to maxJoinNodes"
  | minHistoryNodes > maxHistoryNodes = Just "minHistoryNodes must be less than or equal to maxHistoryNodes"
  | minEndNodes < 1 = Just "you likely want to have at least one EndNode in the chart"
  | maxRegions > 10 = Just "you likely want to have less than 11 Regions in the chart"
  | maxNormalStates > 15 = Just "you likely want to have less than 16 NormalStates in the chart"
  | maxComponentNames > 15 = Just "you likely want to have less than 16 ComponentNames in the chart"
  | maxEndNodes > 10 = Just "you likely want to have less than 11 EndNodes in the chart"
  | maxForkNodes > 3 = Just "you likely want to have less than 4 ForkNodes in the chart"
  | maxJoinNodes > 3 = Just "you likely want to have less than 4 JoinNodes in the chart"
  | maxHistoryNodes > 5 = Just "you likely want to have less than 6 HistoryNodes in the chart"
  | minFlows > maxFlows = Just "minFlows must be less than or equal to maxFlows"
  | otherwise = Nothing

sdConfigToAlloy :: Int -> Int -> SDConfig -> String
sdConfigToAlloy scope bitwidth SDConfig { maxRegionStates
                                        , maxHierarchicalStates
                                        , maxRegions
                                        , maxNormalStates
                                        , maxComponentNames
                                        , maxEndNodes
                                        , maxForkNodes
                                        , maxJoinNodes
                                        , maxHistoryNodes
                                        , maxFlows
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

run {} for #{scope} but #{bitwidth} Int, #{maxRegionStates} RegionsStates, exactly #{maxHierarchicalStates} HierarchicalStates,
#{maxRegions} Regions, exactly #{maxNormalStates} NormalStates, #{maxComponentNames} ComponentNames, #{maxEndNodes} EndNodes,
#{maxForkNodes} ForkNodes, #{maxJoinNodes} JoinNodes, #{maxHistoryNodes} HistoryNodes, exactly #{maxFlows} Flows
    |]


