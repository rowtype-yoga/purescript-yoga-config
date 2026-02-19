module Yoga.Config
  ( readYAML
  , readTOML
  , loadConfig
  , loadConfigFromSources
  , module Yoga.Config.Source
  , module Yoga.Config.Types
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Foldable (foldl)
import Data.Newtype (un)
import Effect.Aff (Aff)
import Yoga.Config.Source (ConfigSource(..), fromEnvPrefix, fromFile, fromFileOptional, fromJSONFile, fromJSONString, fromProfileDir, fromRecord, fromTOMLFile, fromTOMLString, fromYAMLFile, fromYAMLString, override, withEnvResolution)
import Yoga.Config.Types (Duration(..), Bytes(..), Port(..), Secret, mkSecret, revealSecret, NonBlank, mkNonBlank, unNonBlank, bytesToNumber)
import Yoga.Config.TOML (parseTOML)
import Yoga.Config.YAML (parseYAML)
import Yoga.JSON (class ReadForeign, E, readImpl)

readYAML :: forall @a. ReadForeign a => String -> E a
readYAML = runExcept <<< (readImpl <=< parseYAML)

readTOML :: forall @a. ReadForeign a => String -> E a
readTOML = runExcept <<< (readImpl <=< parseTOML)

loadConfig :: forall @a. ReadForeign a => ConfigSource -> Aff (E a)
loadConfig source = do
  foreign_ <- un ConfigSource source
  pure (runExcept (readImpl foreign_))

loadConfigFromSources :: forall @a. ReadForeign a => Array ConfigSource -> Aff (E a)
loadConfigFromSources sources = do
  let merged = foldl override (fromRecord {}) sources
  loadConfig merged
