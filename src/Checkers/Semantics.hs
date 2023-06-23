{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}

module Checkers.Semantics ( checkSemantics ) where

import Datatype (
  Connection(..),
  UMLStateDiagram,
  unUML,
  globalise,
  )

import Checkers.Helpers (checkEmptyOutTran, checkSameOutTran, notJoint)

checkSemantics :: UMLStateDiagram Int -> Maybe String
checkSemantics a
  | not (checkSameConnection a) =
      Just "Error: No two connections are allowed leaving the same source and and having the same label (except From Joint Node)."
  | not (checkEmptyTran a) =
      Just "Error: The non-Joint state which has more than one outgoing connection, must have a non-empty transition label."
  | otherwise =
      Nothing

checkSameConnection :: UMLStateDiagram Int -> Bool
checkSameConnection =
  unUML (\_ sub conn _ ->
           let
             withoutJoint = filter ((`notJoint` sub) . pointFrom) conn
           in
             all (`checkSameOutTran` withoutJoint) withoutJoint
        )
  . globalise

checkEmptyTran :: UMLStateDiagram Int -> Bool
checkEmptyTran =
  unUML (\_ sub conn _ ->
           let
             withoutJoint = filter ((`notJoint` sub) . pointFrom) conn
           in
             all (`checkEmptyOutTran` withoutJoint) withoutJoint
         )
  . globalise
