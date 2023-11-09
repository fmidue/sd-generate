{-# OPTIONS_GHC -Wno-error=incomplete-patterns #-}

module Modelling.StateDiagram.Checkers.Semantics ( checkSemantics ) where

import Modelling.StateDiagram.Datatype (
  Connection(..),
  UMLStateDiagram,
  unUML,
  globalise,
  )

import Modelling.StateDiagram.Checkers.Helpers (checkEmptyOutTran, checkSameOutTran, notForkOrJoin)

checkSemantics :: UMLStateDiagram n Int -> Maybe String
checkSemantics a
  | not (checkSameConnection a) =
      Just "Error: No two connections are allowed leaving the same source and and having the same label (except From ForkOrJoin Node)."
  | not (checkEmptyTran a) =
      Just "Error: The non-ForkOrJoin state which has more than one outgoing connection, must have a non-empty transition label."
  | otherwise =
      Nothing

checkSameConnection :: UMLStateDiagram n Int -> Bool
checkSameConnection =
  unUML (\_ sub conn _ ->
           let
             withoutForkOrJoin = filter ((`notForkOrJoin` sub) . pointFrom) conn
           in
             all (`checkSameOutTran` withoutForkOrJoin) withoutForkOrJoin
        )
  . globalise

checkEmptyTran :: UMLStateDiagram n Int -> Bool
checkEmptyTran =
  unUML (\_ sub conn _ ->
           let
             withoutForkOrJoin = filter ((`notForkOrJoin` sub) . pointFrom) conn
           in
             all (`checkEmptyOutTran` withoutForkOrJoin) withoutForkOrJoin
         )
  . globalise
