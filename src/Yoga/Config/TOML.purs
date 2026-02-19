module Yoga.Config.TOML (parseTOML) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Identity (Identity(..))
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, ForeignError(..))

foreign import _parseTOML :: EU.EffectFn1 String Foreign

parseTOML :: String -> F Foreign
parseTOML = ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< EU.runEffectFn1 _parseTOML
  where
  runPure = unsafePerformEffect
