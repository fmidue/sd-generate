{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Main where

import Modelling.StateDiagram.AlloyDiagrams (render)
import Modelling.StateDiagram.Datatype (UMLStateDiagram(unUML'), StateDiagram(label))
import Modelling.StateDiagram.Example (positiveExamples)
import Control.Monad (forM_)

main :: IO ()
main = forM_ positiveExamples $
  \(file, sd) ->
    writeFile (file ++ ".als") $
    render
    (label (unUML' sd)) -- temporary hack for manual ProtoFlows scope setting
    sd
