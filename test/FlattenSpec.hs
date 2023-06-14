{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module FlattenSpec (
  spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Datatype (StateDiagram'(..), Connection' (..), UMLStateDiagram)
import Example (flatCase2, flatCase1)
import Flatten (flatten)

flatCase1Res :: UMLStateDiagram
flatCase1Res
  = StateDiagram {substate =
    [InnerMostState {label = 1, name = "A", operations = ""}
    ,InnerMostState {label = 2, name = "B", operations = ""}
    ,InnerMostState {label = 3, name = "C", operations = ""}
    ,InnerMostState {label = 4, name = ", G", operations = ""}
    ,InnerMostState {label = 5, name = ", H", operations = ""}]
    , label = 0, name = ""
    , connection =
    [Connection {pointFrom = [1], pointTo = [5], transition = "a"}
    ,Connection {pointFrom = [4], pointTo = [2], transition = "c"}
    ,Connection {pointFrom = [2], pointTo = [3], transition = "d"}
    ,Connection {pointFrom = [3], pointTo = [4], transition = "e"}
    ,Connection {pointFrom = [5], pointTo = [4], transition = "b"}]
    , startState = [1]}

flatCase2Res :: UMLStateDiagram
flatCase2Res
  = StateDiagram {substate =
    [InnerMostState {label = 1, name = "A, D", operations = "\n"}
    ,InnerMostState {label = 3, name = "A, E", operations = "\n"}
    ,InnerMostState {label = 6, name = "B, D", operations = "\n"}
    ,InnerMostState {label = 7, name = "B, E", operations = "\n"}
    ,InnerMostState {label = 8, name = "C, D", operations = "\n"}
    ,InnerMostState {label = 9, name = "C, E", operations = "\n"}
    ,InnerMostState {label = 2, name = "F", operations = ""}
    ,InnerMostState {label = 4, name = ", H", operations = ""}
    ,InnerMostState {label = 10, name = ", G", operations = ""}]
    , label = 22, name = "", connection =
    [Connection {pointFrom = [9], pointTo = [2], transition = "c"}
    ,Connection {pointFrom = [2], pointTo = [4], transition = "c"}
    ,Connection {pointFrom = [2], pointTo = [4], transition = "d"}
    ,Connection {pointFrom = [4], pointTo = [3], transition = "z"}
    ,Connection {pointFrom = [10], pointTo = [1], transition = "z"}
    ,Connection {pointFrom = [1], pointTo = [6], transition = "a"}
    ,Connection {pointFrom = [1], pointTo = [3], transition = "b"}
    ,Connection {pointFrom = [3], pointTo = [7], transition = "a"}
    ,Connection {pointFrom = [3], pointTo = [1], transition = "b"}
    ,Connection {pointFrom = [6], pointTo = [9], transition = "b"}
    ,Connection {pointFrom = [7], pointTo = [8], transition = "b"}
    ,Connection {pointFrom = [8], pointTo = [1], transition = "a"}
    ,Connection {pointFrom = [8], pointTo = [9], transition = "b"}
    ,Connection {pointFrom = [9], pointTo = [3], transition = "a"}
    ,Connection {pointFrom = [9], pointTo = [8], transition = "b"}
    ,Connection {pointFrom = [4], pointTo = [10], transition = "d"}]
    , startState = [1]}

spec :: Spec
spec = do
  describe "flatten regions" $
    prop "flatten flatCase1" $
      flatten flatCase1 == flatCase1Res
      -- (flatten flatCase2) `shouldBe` flattedCase2 (TODO: fix multidim rewire of converted joints)
      -- in this case C, E must go to F and not C, D to F like observable right now
