module Yoga.Config.YAML (parseYAML) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Identity (Identity(..))
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, ForeignError(..))

foreign import _parseYAML :: EU.EffectFn1 String Foreign

parseYAML :: String -> F Foreign
parseYAML = ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< EU.runEffectFn1 _parseYAML
  where
  runPure = unsafePerformEffect
