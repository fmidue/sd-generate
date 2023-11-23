{-# LANGUAGE TypeApplications #-}
-- | Common functions for application modules
module Common (
  forceErrors,
  instanceInput,
  printNetAndInfo,
  renderPetriNet,
  withLang,
  ) where

import qualified Control.Monad.Output.Generic     as GenericOutput (withLang)

import Control.Monad.Output             (LangM', Language, ReportT)
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Maybe                       (fromMaybe)
import Diagrams.Prelude                 (Diagram, mkWidth)
import Diagrams.Backend.SVG             (B, renderSVG)

instanceInput :: IO Int
instanceInput = do
  putStr "Seed of wanted Instance: "
  read <$> getLine

forceErrors :: Monad m => ExceptT String m () -> m ()
forceErrors m = do
  () <- either error id <$> runExceptT m
  return ()

printNetAndInfo :: Show a => String -> (Diagram B, a) -> IO ()
printNetAndInfo x (dia, concurrent) = do
  renderPetriNet x dia
  print concurrent

renderPetriNet :: String -> Diagram B -> IO ()
renderPetriNet x dia = do
  renderSVG name (mkWidth 800) dia
  putStrLn $ "wrote file" ++ name
  where
    name = x ++ "petri.svg"

withLang :: LangM' (ReportT (IO ()) IO) a -> Language -> IO a
withLang x l =
  fromMaybe (error "failed")
  <$> GenericOutput.withLang @Language @(ReportT (IO ()) IO) x l
