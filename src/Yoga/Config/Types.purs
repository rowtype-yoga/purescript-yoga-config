module Yoga.Config.Types
  ( Duration(..)
  , Bytes(..)
  , Port(..)
  , Secret
  , mkSecret
  , revealSecret
  , NonBlank
  , mkNonBlank
  , unNonBlank
  , bytesToNumber
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Data.Either (note)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.String.CodePoints (length, stripSuffix)
import Data.String.Common (trim)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (class Duration, Milliseconds(..))
import Foreign (ForeignError(..), readInt, readNumber, readString)
import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)

foreign import showNumber :: Number -> String

-- | A duration parsed from strings like "500ms", "5s", "10m", "2h", "1d"
newtype Duration = Duration Milliseconds

derive instance Newtype Duration _
derive newtype instance Eq Duration
derive newtype instance Ord Duration
derive newtype instance Show Duration

instance Duration Duration where
  fromDuration (Duration ms) = ms
  toDuration ms = Duration ms

instance ReadForeign Duration where
  readImpl f = do
    str <- readString f
    parseDuration (trim str)
      # note (singleton (ForeignError ("Invalid duration: " <> show str <> ". Expected e.g. \"500ms\", \"5s\", \"10m\", \"2h\", \"1d\"")))
      # except

instance WriteForeign Duration where
  writeImpl (Duration (Milliseconds ms)) = writeImpl (formatDuration ms)
    where
    formatDuration n
      | n >= 86400000.0 && isWhole (n / 86400000.0) = showNumber (n / 86400000.0) <> "d"
      | n >= 3600000.0 && isWhole (n / 3600000.0) = showNumber (n / 3600000.0) <> "h"
      | n >= 60000.0 && isWhole (n / 60000.0) = showNumber (n / 60000.0) <> "m"
      | n >= 1000.0 && isWhole (n / 1000.0) = showNumber (n / 1000.0) <> "s"
      | otherwise = showNumber n <> "ms"
    isWhole x = Number.floor x == x

parseDuration :: String -> Maybe Duration
parseDuration str = tryMs <|> tryS <|> tryM <|> tryH <|> tryD
  where
  tryMs = withSuffix "ms" str <#> \n -> Duration (Milliseconds n)
  tryS = withSuffix "s" str <#> \n -> Duration (Milliseconds (n * 1000.0))
  tryM = withSuffix "m" str <#> \n -> Duration (Milliseconds (n * 60000.0))
  tryH = withSuffix "h" str <#> \n -> Duration (Milliseconds (n * 3600000.0))
  tryD = withSuffix "d" str <#> \n -> Duration (Milliseconds (n * 86400000.0))

  withSuffix suffix s = do
    numStr <- stripSuffix (Pattern suffix) s
    n <- Number.fromString (trim numStr)
    if n >= 0.0 then Just n else Nothing

-- | Bytes parsed from strings like "100B", "10KB", "5MB", "2GB", "1TB"
newtype Bytes = Bytes Number

derive instance Newtype Bytes _
derive newtype instance Eq Bytes
derive newtype instance Ord Bytes
derive newtype instance Show Bytes

instance ReadForeign Bytes where
  readImpl f = readAsNumber f <|> readAsString f
    where
    readAsNumber f' = do
      n <- readNumber f'
      pure (Bytes n)

    readAsString f' = do
      str <- readString f'
      parseBytes (trim str)
        # note (singleton (ForeignError ("Invalid bytes: " <> show str <> ". Expected e.g. \"100B\", \"10KB\", \"5MB\", \"2GB\", \"1TB\"")))
        # except

instance WriteForeign Bytes where
  writeImpl (Bytes n) = writeImpl (formatBytes n)
    where
    formatBytes b
      | b >= 1099511627776.0 && isWhole (b / 1099511627776.0) = showNumber (b / 1099511627776.0) <> "TB"
      | b >= 1073741824.0 && isWhole (b / 1073741824.0) = showNumber (b / 1073741824.0) <> "GB"
      | b >= 1048576.0 && isWhole (b / 1048576.0) = showNumber (b / 1048576.0) <> "MB"
      | b >= 1024.0 && isWhole (b / 1024.0) = showNumber (b / 1024.0) <> "KB"
      | otherwise = showNumber b <> "B"
    isWhole x = Number.floor x == x

bytesToNumber :: Bytes -> Number
bytesToNumber (Bytes n) = n

parseBytes :: String -> Maybe Bytes
parseBytes str = tryTB <|> tryGB <|> tryMB <|> tryKB <|> tryB
  where
  tryTB = withSuffix "TB" str <#> \n -> Bytes (n * 1099511627776.0)
  tryGB = withSuffix "GB" str <#> \n -> Bytes (n * 1073741824.0)
  tryMB = withSuffix "MB" str <#> \n -> Bytes (n * 1048576.0)
  tryKB = withSuffix "KB" str <#> \n -> Bytes (n * 1024.0)
  tryB = withSuffix "B" str <#> \n -> Bytes n

  withSuffix suffix s = do
    numStr <- stripSuffix (Pattern suffix) s
    n <- Number.fromString (trim numStr)
    if n >= 0.0 then Just n else Nothing

-- | A port number (0-65535), parsed from an integer
newtype Port = Port Int

derive instance Newtype Port _
derive newtype instance Eq Port
derive newtype instance Ord Port
derive newtype instance Show Port

instance ReadForeign Port where
  readImpl f = do
    n <- readInt f
    if n >= 0 && n <= 65535 then pure (Port n)
    else except $ note (singleton (ForeignError ("Port out of range: " <> show n <> ". Expected 0-65535"))) Nothing

instance WriteForeign Port where
  writeImpl (Port n) = writeImpl n

-- | A secret value that masks itself in Show to prevent accidental logging.
-- | Use `revealSecret` to access the underlying value.
newtype Secret a = Secret a

derive instance Eq a => Eq (Secret a)
derive instance Ord a => Ord (Secret a)

instance Show (Secret a) where
  show _ = "Secret(***)"

instance ReadForeign a => ReadForeign (Secret a) where
  readImpl f = Secret <$> readImpl f

instance WriteForeign a => WriteForeign (Secret a) where
  writeImpl (Secret a) = writeImpl a

mkSecret :: forall a. a -> Secret a
mkSecret = Secret

revealSecret :: forall a. Secret a -> a
revealSecret (Secret a) = a

-- | A non-blank string (not empty, not just whitespace).
newtype NonBlank = NonBlank String

derive instance Eq NonBlank
derive instance Ord NonBlank

instance Show NonBlank where
  show (NonBlank s) = "(NonBlank " <> show s <> ")"

instance ReadForeign NonBlank where
  readImpl f = do
    str <- readString f
    let trimmed = trim str
    if length trimmed > 0 then pure (NonBlank trimmed)
    else except $ note (singleton (ForeignError "Expected non-blank string, got empty or whitespace")) Nothing

instance WriteForeign NonBlank where
  writeImpl (NonBlank s) = writeImpl s

mkNonBlank :: String -> Maybe NonBlank
mkNonBlank s = do
  let trimmed = trim s
  if length trimmed > 0 then Just (NonBlank trimmed)
  else Nothing

unNonBlank :: NonBlank -> String
unNonBlank (NonBlank s) = s
