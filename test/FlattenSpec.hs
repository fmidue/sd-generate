{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deprecations   #-}
module FlattenSpec (
  spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Datatype (StateDiagram(..)
                ,Connection (..)
                ,UMLStateDiagram (unUML')
                ,umlStateDiagram
                ,unUML
                -- ,unUML'
                )
import Example (flatCase2
               ,flatCase1
               ,flatCase3
               ,positiveExamples)
import Flatten (flatten
               -- ,allVertexAddresses
               -- ,vertexPath
               )

flatCase1Res :: UMLStateDiagram [String] Int
flatCase1Res
  = umlStateDiagram $ StateDiagram {substates =
    [InnerMostState {label = 1, name = ["P","G"], operations = ""}
    ,InnerMostState {label = 2, name = ["P","H"], operations = ""}
    ,InnerMostState {label = 3, name = ["A"], operations = ""}
    ,InnerMostState {label = 4, name = ["B"], operations = ""}
    ,InnerMostState {label = 5, name = ["C"], operations = ""}]
    , label = 0, name = [""]
    , connections =
    [Connection {pointFrom = [3], pointTo = [2], transition = "a"}
    ,Connection {pointFrom = [1], pointTo = [4], transition = "c"}
    ,Connection {pointFrom = [4], pointTo = [5], transition = "d"}
    ,Connection {pointFrom = [5], pointTo = [1], transition = "e"}
    ,Connection {pointFrom = [2], pointTo = [1], transition = "b"}]
    , startState = [2]}

flatCase3Res1Step :: UMLStateDiagram [String] Int
flatCase3Res1Step
  = umlStateDiagram $ StateDiagram { substates = [
    InnerMostState { label = 1, name = ["P1", "G"], operations = "" }
    ,InnerMostState { label = 2, name = ["P1", "H"], operations = "" }
    ,InnerMostState { label = 3, name = ["A"], operations = "" }
    ,InnerMostState { label = 4, name = ["B"], operations = "" }
    ,InnerMostState { label = 5, name = ["C"], operations = "" }
    ,StateDiagram { label = 6
                  , name = ["P2"]
                  , startState = [2]
                  , connections = []
                  , substates = [InnerMostState { label = 1, name = ["I"], operations = ""}
                                ,InnerMostState { label = 2, name = ["J"], operations = ""}]
    }]
    ,startState = [2]
    ,label = error "not relevant"
    ,name = [""]
    ,connections =
    [Connection {pointFrom = [3], pointTo = [2], transition = "a"}
    ,Connection {pointFrom = [1], pointTo = [4], transition = "c"}
    ,Connection {pointFrom = [4], pointTo = [5], transition = "d"}
    ,Connection {pointFrom = [5], pointTo = [1], transition = "e"}
    ,Connection {pointFrom = [2], pointTo = [1], transition = "b"}
    ,Connection {pointFrom = [6,1], pointTo = [6,2], transition = "i"}]}
{-
resolveVertices :: [([[Int]], [[Int]])]
resolveVertices
  = let
    diagrams = map (unUML' . snd) positiveExamples
    allLabels = map allVertexAddresses diagrams
    in
    zip (map (map (\(vertex,path) -> map label path ++ [label vertex]))
      (zipWith (\labels chart -> map (`vertexPath` chart) labels) allLabels diagrams))
    allLabels
-}

spec :: Spec
spec
  = do
    describe "flatten tests" $ do
      -- it "attempting to resolve available state chart vertices shall not fail" $ do
      --  all (uncurry (==)) resolveVertices `shouldBe` True
      it "flatten flatCase1" $ do
        let result = flatten flatCase1
        result `shouldBe` flatCase1Res
      it "flatten flatCase3 - lift one hierarchical state correctly" $ do
        let result = flatten flatCase3
        result `shouldBe` flatCase3Res1Step
      it "isStructurallySameAs" $ do
        let result = isStructurallySameAs flatCase1Res flatCase1Res
        result `shouldBe` True
{-
      it "TODO: relabel connections by relation" $ do
        let result = True
        result `shouldBe` True
-}


{- lets assume for simplicity that we only have one layer for now that we want to verify.
   this avoids the necessity for globalizing any connections or having to map through
   hierarchies as everything we care about is a single list of substates, a list of connections
   and one startState.
-}

isStructurallySameAs :: (Eq n, Eq l) => UMLStateDiagram n l -> UMLStateDiagram n l -> Bool
isStructurallySameAs g1 g2
  = let
    {- r = [(l1,l2)|InnerMostState{ label = l1
                               , name = n1 } <- unUML (\substate _ _ _ -> substate) g1
        , InnerMostState{ label = l2
                        , name = n2 } <- unUML (\substate _ _ _ -> substate) g2
        , n1 == n2] -}
    in
    length (unUML (\_ substate _ _ -> substate) g1) == length (unUML (\_ substate _ _ -> substate) g2)

{-
      it "@pre: all left and right labels on nodes are unique" $ do
        let result = True
        result `shouldBe` True

      it "@post all nodes have unique Left labels" $ do
        let result = True
        result `shouldBe` True

      it "@post all connections pointFrom, pointTo references are build of lefts" $ do
        let result = True
        result `shouldBe` True

      it "@post quantity of connections remains unchanged" $ do
        let result = True
        result `shouldBe` True

      it "@post nodes remain in relation" $ do
        let result = True
        result `shouldBe` True
-}


{-
   concept to verify that making labels unique across a layer works as expected
   pre: both Left and Right labels might be present at nodes and used within connections
        all Left and Right labels on nodes are unique
   post: nodes and connections only have Left labels
         every node has its own, unique Left label (i.e. no Left label is used more than once)
         nodes that have been in relation to each other through their labels remain in relation to each other.
         the total amount of connections stays unchanged, (i.e. there are no new relations between nodes)
         (or any relations lost)
 -}

{- a flattened diagram has the same localized and globalized representation
   because it has only one layer. -}

{- flattening two charts will allow to compare them easily -}

{- generate diagram; flatten, and output pre and post image to use it as task
   further, globalize, collect transition literals, generate walkable sequences of defined length
   and print the chain of active configurations to use it as task as well. -}
