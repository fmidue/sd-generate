{-# OPTIONS_GHC -Wno-error=orphans #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Datatype.ClassInstances () where

import Datatype (UMLStateDiagram, unUML, StateDiagram(..))
import Generic.Functor (GenericFunctor(..))

deriving via GenericFunctor UMLStateDiagram instance Functor UMLStateDiagram

instance Eq a => Eq (UMLStateDiagram a) where
  sd1 == sd2
    =
    unUML
    (\n1 s1 c1 ss1 ->
        unUML
        (\n2 s2 c2 ss2 ->
           n1 == n2 && s1 == s2 && c1 == c2 && ss1 == ss2)
        sd2)
    sd1

instance Show a => Show (UMLStateDiagram a) where
  show d =
    unUML
    (\name substate connection startState ->
        show $ StateDiagram { label = Hidden, .. }
    )
    (fmap Visible d)

data HideRootLabel a = Hidden | Visible a

instance Show a => Show (HideRootLabel a) where
  show Hidden = "THIS LABEL IS IRRELEVANT AND THUS HIDDEN!"
  show (Visible a) = show a
