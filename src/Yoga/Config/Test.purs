module Yoga.Config.Test
  ( validateConfig
  , validateYAML
  , validateTOML
  , validateJSON
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.List.NonEmpty (toList)
import Effect.Aff (Aff)
import Foreign (renderForeignError)
import Yoga.Config (readYAML, readTOML, loadConfig)
import Yoga.Config.Source (ConfigSource)
import Yoga.JSON (class ReadForeign, E, readJSON)

validateConfig :: forall @a. ReadForeign a => ConfigSource -> Aff (Either String a)
validateConfig source = do
  result <- loadConfig source
  pure (formatErrors result)

validateYAML :: forall @a. ReadForeign a => String -> Either String a
validateYAML = formatErrors <<< readYAML

validateTOML :: forall @a. ReadForeign a => String -> Either String a
validateTOML = formatErrors <<< readTOML

validateJSON :: forall @a. ReadForeign a => String -> Either String a
validateJSON = formatErrors <<< readJSON

formatErrors :: forall a. E a -> Either String a
formatErrors = lmap (intercalate "\n" <<< map renderForeignError <<< toList)
