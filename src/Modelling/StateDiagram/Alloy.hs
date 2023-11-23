{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Modelling.StateDiagram.Alloy(alloySDGenerator
                                   ,componentsSigRules
                                   ,startstateRules
                                   ,endstateRules
                                   ,regionRules
                                   ,nodeRules
                                   ,reachabilityRules
                                   ,historyRules
                                   ,transitionRules
                                   ,substateRules
                                   ,nameRules
                                   ,trueReachability
                                   ,scenarioCases)
where

import Data.FileEmbed (embedStringFile)


{- failsafe query -}
alloySDGenerator :: String
alloySDGenerator
  = $(embedStringFile "./alloy/sd/uml_state_diagram.als")

{- in analogy to the other config files; the Alloy rules for state charts are statically compiled into the binary -}

componentsSigRules :: String
componentsSigRules = unlines $ drop 2 $ lines $(embedStringFile "./alloy/sd/components_sig.als")

startstateRules :: String
startstateRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/startstate_rules.als")

endstateRules :: String
endstateRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/endstate_rules.als")

regionRules :: String
regionRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/region_rules.als")

nodeRules :: String
nodeRules = unlines $ drop 4 $ lines  $(embedStringFile "./alloy/sd/node_rules.als")

reachabilityRules :: String
reachabilityRules = unlines $ drop 5 $ lines $(embedStringFile "./alloy/sd/reachability_rules.als")

historyRules :: String
historyRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/history_rules.als")

transitionRules :: String
transitionRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/transition_rules.als")

substateRules :: String
substateRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/substate_rules.als")

nameRules :: String
nameRules = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/name_rules.als")

trueReachability :: String
trueReachability = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/trueReachability.als")

scenarioCases :: String
scenarioCases = unlines $ drop 4 $ lines $(embedStringFile "./alloy/sd/examples_based_on_version_4/teaching_examples.als")
