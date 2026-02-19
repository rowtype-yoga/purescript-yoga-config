module Yoga.Config.OmLayer
  ( ConfigL
  , ConfigError
  , configLayer
  , configLayerFromSources
  , configLayer'
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Newtype (un)
import Foreign (MultipleErrors)
import Yoga.Config.Source (ConfigSource(..), fromRecord, override)
import Yoga.JSON (class ReadForeign, readImpl)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer)

type ConfigL :: forall k. k -> Row k -> Row k
type ConfigL config r = (config :: config | r)

type ConfigError r = (configParseError :: { errors :: MultipleErrors } | r)

configLayer
  :: forall @config
   . ReadForeign config
  => ConfigSource
  -> OmLayer () (ConfigError ()) (Record (ConfigL config ()))
configLayer source = makeLayer do
  foreign_ <- un ConfigSource source # Om.fromAff
  case runExcept (readImpl foreign_) of
    Left errors -> Om.throw { configParseError: { errors } }
    Right config -> pure { config }

configLayerFromSources
  :: forall @config
   . ReadForeign config
  => Array ConfigSource
  -> OmLayer () (ConfigError ()) (Record (ConfigL config ()))
configLayerFromSources sources = do
  let merged = foldl override (fromRecord {}) sources
  configLayer merged

configLayer'
  :: forall @config
   . config
  -> OmLayer () () (Record (ConfigL config ()))
configLayer' config = makeLayer (pure { config })
