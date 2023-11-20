{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}

module Modelling.StateDiagram.Checkers.Semantics ( checkSemantics ) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  UMLStateDiagram,
  unUML,
  globalise,
  )

import Modelling.StateDiagram.Checkers.Helpers (checkEmptyOutTran, checkSameOutTran, isFork)

checkSemantics :: UMLStateDiagram n Int -> Maybe String
checkSemantics a
  | not (checkSameConnection a) =
      Just "Error: No two connections are allowed leaving the same source and having the same label (except from Fork node)."
  | not (checkEmptyTran a) =
      Just "Error: Non-Fork nodes which have more than one outgoing connection must not have an empty transition label there."
  | otherwise =
      Nothing

checkSameConnection :: UMLStateDiagram n Int -> Bool
checkSameConnection =
  unUML (\_ sub conn _ ->
           let
             withoutFromFork = filter (not . (`isFork` sub) . pointFrom) conn
           in
             all (`checkSameOutTran` withoutFromFork) withoutFromFork
        )
  . globalise

checkEmptyTran :: UMLStateDiagram n Int -> Bool
checkEmptyTran =
  unUML (\_ sub conn _ ->
           let
             withoutFromFork = filter (not . (`isFork` sub) . pointFrom) conn
           in
             all (`checkEmptyOutTran` withoutFromFork) withoutFromFork
         )
  . globalise
