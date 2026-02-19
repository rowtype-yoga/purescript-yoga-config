module Yoga.Config.Source
  ( ConfigSource(..)
  , fromEnvPrefix
  , fromYAMLString
  , fromTOMLString
  , fromJSONString
  , fromFile
  , fromYAMLFile
  , fromTOMLFile
  , fromJSONFile
  , fromRecord
  , override
  , withEnvResolution
  , fromFileOptional
  , fromProfileDir
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign, unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (access, readTextFile)
import Node.Process as Process
import Yoga.Config.TOML (parseTOML)
import Yoga.Config.YAML (parseYAML)
import Yoga.JSON (class WriteForeign, parseJSON, writeImpl)

newtype ConfigSource = ConfigSource (Aff Foreign)

derive instance Newtype ConfigSource _

instance Semigroup ConfigSource where
  append = override

instance Monoid ConfigSource where
  mempty = fromRecord {}

foreign import _getEnvWithPrefix :: EffectFn1 String Foreign

fromEnvPrefix :: String -> ConfigSource
fromEnvPrefix prefix = ConfigSource do
  runEffectFn1 _getEnvWithPrefix prefix # liftEffect

fromYAMLString :: String -> ConfigSource
fromYAMLString str = ConfigSource do
  runExcept (parseYAML str)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromTOMLString :: String -> ConfigSource
fromTOMLString str = ConfigSource do
  runExcept (parseTOML str)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromJSONString :: String -> ConfigSource
fromJSONString str = ConfigSource do
  runExcept (parseJSON str)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromFile :: String -> ConfigSource
fromFile path = case fileExtension path of
  ".yaml" -> fromYAMLFile path
  ".yml" -> fromYAMLFile path
  ".toml" -> fromTOMLFile path
  ".json" -> fromJSONFile path
  ext -> ConfigSource (Aff.throwError (error ("Unknown config file extension: " <> ext)))

fromYAMLFile :: String -> ConfigSource
fromYAMLFile path = ConfigSource do
  contents <- readTextFile UTF8 path
  runExcept (parseYAML contents)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromTOMLFile :: String -> ConfigSource
fromTOMLFile path = ConfigSource do
  contents <- readTextFile UTF8 path
  runExcept (parseTOML contents)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromJSONFile :: String -> ConfigSource
fromJSONFile path = ConfigSource do
  contents <- readTextFile UTF8 path
  runExcept (parseJSON contents)
    # either (\errs -> Aff.throwError (error (show errs))) pure

fromRecord :: forall a. WriteForeign a => a -> ConfigSource
fromRecord a = ConfigSource (pure (writeImpl a))

override :: ConfigSource -> ConfigSource -> ConfigSource
override (ConfigSource base) (ConfigSource over) = ConfigSource do
  b <- base
  o <- over
  pure (deepMerge b o)

foreign import _resolveEnvRefs :: EffectFn1 Foreign Foreign

withEnvResolution :: ConfigSource -> ConfigSource
withEnvResolution (ConfigSource aff) = ConfigSource do
  foreign_ <- aff
  runEffectFn1 _resolveEnvRefs foreign_ # liftEffect

foreign import _deepMerge :: Foreign -> Foreign -> Foreign

deepMerge :: Foreign -> Foreign -> Foreign
deepMerge = _deepMerge

fromFileOptional :: String -> ConfigSource
fromFileOptional path = ConfigSource do
  err <- access path
  case err of
    Just _ -> pure (unsafeToForeign {})
    Nothing -> un ConfigSource (fromFile path)

fromProfileDir :: { dir :: String, ext :: String, envVar :: String, default :: String } -> ConfigSource
fromProfileDir { dir, ext, envVar, default: defaultProfile } = ConfigSource do
  profile <- Process.lookupEnv envVar # liftEffect # map (fromMaybe defaultProfile)
  let base = fromFileOptional (dir <> "/default" <> ext) # withEnvResolution
  let profileSource = fromFileOptional (dir <> "/" <> profile <> ext) # withEnvResolution
  un ConfigSource (override base profileSource)

fileExtension :: String -> String
fileExtension path = case String.lastIndexOf (String.Pattern ".") path of
  Nothing -> ""
  Just i -> String.toLower (String.drop i path)
