module Test.Main where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Node.Process as Process
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Yoga.Config (readYAML, readTOML, loadConfig, loadConfigFromSources, Duration(..), Bytes(..), Port(..), Secret, mkSecret, revealSecret, NonBlank, mkNonBlank, unNonBlank, bytesToNumber)
import Yoga.Config.OmLayer (configLayer, configLayerFromSources, configLayer')
import Yoga.Config.Source (fromYAMLString, fromTOMLString, fromJSONString, fromRecord, fromEnvPrefix, fromFile, fromFileOptional, fromTOMLFile, fromJSONFile, fromProfileDir, override, withEnvResolution)
import Yoga.Config.Test (validateConfig, validateYAML, validateTOML, validateJSON)
import Yoga.Om as Om
import Yoga.Om.Layer (runLayer)

type AppConfig =
  { host :: String
  , port :: Int
  , database :: { name :: String, pool :: Int }
  }

type ServerConfig =
  { host :: NonBlank
  , port :: Port
  , timeout :: Duration
  , maxUpload :: Bytes
  }

type DatabaseConfig =
  { host :: String
  , port :: Int
  , name :: NonBlank
  , password :: Maybe (Secret String)
  , pool :: Int
  }

type FullConfig =
  { server :: ServerConfig
  , database :: DatabaseConfig
  , app :: { name :: NonBlank, debug :: Boolean }
  }

type Error = Aff.Error

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do

  describe "readYAML" do
    it "parses YAML into a typed record" do
      case readYAML @AppConfig "host: localhost\nport: 8080\ndatabase:\n  name: mydb\n  pool: 10\n" of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 8080
          config.database.name `shouldEqual` "mydb"
          config.database.pool `shouldEqual` 10

    it "returns Left for invalid YAML" do
      readYAML @AppConfig "host: localhost\nport: not_a_number\n" `shouldSatisfy` isLeft

  describe "readTOML" do
    it "parses TOML into a typed record" do
      let toml = "host = \"localhost\"\nport = 8080\n\n[database]\nname = \"mydb\"\npool = 10\n"
      case readTOML @AppConfig toml of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 8080
          config.database.name `shouldEqual` "mydb"
          config.database.pool `shouldEqual` 10

  describe "fromRecord" do
    it "creates a config source from a record" do
      result <- loadConfig @AppConfig (fromRecord { host: "localhost", port: 3000, database: { name: "test", pool: 5 } })
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 3000

  describe "override" do
    it "deep merges config sources with right taking precedence" do
      let base = fromRecord { host: "localhost", port: 3000, database: { name: "dev", pool: 5 } }
      let overrides = fromJSONString """{"port": 5432, "database": {"name": "prod"}}"""
      result <- loadConfig @AppConfig (override base overrides)
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 5432
          config.database.name `shouldEqual` "prod"
          config.database.pool `shouldEqual` 5

  describe "loadConfig" do
    it "loads from YAML string source" do
      result <- loadConfig @AppConfig (fromYAMLString "host: myhost\nport: 9090\ndatabase:\n  name: yamldb\n  pool: 20\n")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "myhost"
          config.port `shouldEqual` 9090

    it "loads from TOML string source" do
      result <- loadConfig @AppConfig (fromTOMLString "host = \"tomlhost\"\nport = 7070\n\n[database]\nname = \"tomldb\"\npool = 15\n")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "tomlhost"
          config.port `shouldEqual` 7070

  describe "loadConfigFromSources" do
    it "merges multiple sources left to right" do
      let defaults = fromRecord { host: "localhost", port: 3000, database: { name: "dev", pool: 5 } }
      let yamlOverride = fromYAMLString "port: 5432\ndatabase:\n  name: staging\n"
      let jsonOverride = fromJSONString """{"database": {"pool": 25}}"""
      result <- loadConfigFromSources @AppConfig [ defaults, yamlOverride, jsonOverride ]
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 5432
          config.database.name `shouldEqual` "staging"
          config.database.pool `shouldEqual` 25

  describe "fromEnvPrefix" do
    it "reads env vars with prefix and nests by underscore" do
      liftEffect do
        Process.setEnv "MYAPP_HOST" "envhost"
        Process.setEnv "MYAPP_PORT" "4000"
      let source = override (fromRecord { host: "localhost", port: 3000 }) (fromEnvPrefix "MYAPP")
      result <- loadConfig @{ host :: String, port :: Int } source
      case result of
        Left _ -> fail "should parse"
        Right config -> config.host `shouldEqual` "envhost"

  describe "withEnvResolution" do
    it "resolves env var references" do
      liftEffect $ Process.setEnv "CFG_HOST" "from-env"
      result <- loadConfig @{ host :: String, port :: Int } (fromYAMLString "host:\n  env: CFG_HOST\nport: 3000\n" # withEnvResolution)
      case result of
        Left _ -> fail "should parse"
        Right config -> config.host `shouldEqual` "from-env"

    it "uses default when env var is missing" do
      liftEffect $ Process.unsetEnv "CFG_MISSING"
      result <- loadConfig @{ host :: String, port :: Int } (fromYAMLString "host:\n  env: CFG_MISSING\n  default: default-host\nport: 3000\n" # withEnvResolution)
      case result of
        Left _ -> fail "should parse"
        Right config -> config.host `shouldEqual` "default-host"

    it "plain inline values pass through unchanged" do
      result <- loadConfig @{ host :: String, port :: Int } (fromYAMLString "host: plain-host\nport: 3000\n" # withEnvResolution)
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "plain-host"
          config.port `shouldEqual` 3000

    it "resolves nested env refs" do
      liftEffect $ Process.setEnv "CFG_DB_NAME" "env-db"
      result <- loadConfig @{ database :: { name :: String, pool :: Int } } (fromYAMLString "database:\n  name:\n    env: CFG_DB_NAME\n  pool: 10\n" # withEnvResolution)
      case result of
        Left _ -> fail "should parse"
        Right config -> config.database.name `shouldEqual` "env-db"

    it "throws for missing env without default" do
      liftEffect $ Process.unsetEnv "MISSING_REQUIRED_VAR"
      result <- Aff.try (loadConfig @{ host :: String, port :: Int } (fromYAMLString "host:\n  env: MISSING_REQUIRED_VAR\nport: 3000\n" # withEnvResolution))
      case result of
        Right _ -> fail "should throw"
        Left err -> message err `shouldSatisfy` \msg -> String.contains (Pattern "MISSING_REQUIRED_VAR") msg

  describe "Duration" do
    it "parses milliseconds" do
      case readYAML @{ timeout :: Duration } "timeout: \"500ms\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 500.0)

    it "parses seconds" do
      case readYAML @{ timeout :: Duration } "timeout: \"5s\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 5000.0)

    it "parses minutes" do
      case readYAML @{ timeout :: Duration } "timeout: \"10m\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 600000.0)

    it "parses hours" do
      case readYAML @{ timeout :: Duration } "timeout: \"2h\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 7200000.0)

    it "parses days" do
      case readYAML @{ timeout :: Duration } "timeout: \"1d\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 86400000.0)

    it "rejects invalid suffix" do
      readYAML @{ timeout :: Duration } "timeout: \"5x\"\n" `shouldSatisfy` isLeft

    it "rejects non-numeric value" do
      readYAML @{ timeout :: Duration } "timeout: \"abcms\"\n" `shouldSatisfy` isLeft

    it "rejects empty string" do
      readYAML @{ timeout :: Duration } "timeout: \"\"\n" `shouldSatisfy` isLeft

    it "rejects suffix only" do
      readYAML @{ timeout :: Duration } "timeout: \"ms\"\n" `shouldSatisfy` isLeft

    it "parses fractional values" do
      case readYAML @{ timeout :: Duration } "timeout: \"1.5s\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 1500.0)

    it "rejects negative duration" do
      readYAML @{ timeout :: Duration } "timeout: \"-5s\"\n" `shouldSatisfy` isLeft

    it "zero duration is valid" do
      case readYAML @{ timeout :: Duration } "timeout: \"0ms\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` Duration (Milliseconds 0.0)

  describe "Bytes" do
    it "parses kilobytes" do
      case readYAML @{ maxSize :: Bytes } "maxSize: \"10KB\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.maxSize `shouldEqual` Bytes 10240.0

    it "parses megabytes" do
      case readYAML @{ maxSize :: Bytes } "maxSize: \"5MB\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.maxSize `shouldEqual` Bytes 5242880.0

    it "parses gigabytes" do
      case readYAML @{ maxSize :: Bytes } "maxSize: \"2GB\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.maxSize `shouldEqual` Bytes 2147483648.0

    it "parses raw number" do
      case readYAML @{ size :: Bytes } "size: 1024\n" of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` Bytes 1024.0

    it "parses bytes suffix" do
      case readYAML @{ size :: Bytes } "size: \"512B\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` Bytes 512.0

    it "parses terabytes" do
      case readYAML @{ size :: Bytes } "size: \"1TB\"\n" of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` Bytes 1099511627776.0

    it "rejects invalid suffix" do
      readYAML @{ size :: Bytes } "size: invalid\n" `shouldSatisfy` isLeft

    it "bytesToNumber extracts value" do
      bytesToNumber (Bytes 1024.0) `shouldEqual` 1024.0

    it "rejects negative bytes" do
      readYAML @{ size :: Bytes } "size: \"-10KB\"\n" `shouldSatisfy` isLeft

  describe "Port" do
    it "parses valid port" do
      case readYAML @{ port :: Port } "port: 8080\n" of
        Left _ -> fail "should parse"
        Right c -> c.port `shouldEqual` Port 8080

    it "rejects out of range port" do
      readYAML @{ port :: Port } "port: 70000\n" `shouldSatisfy` isLeft

    it "accepts port 0" do
      case readYAML @{ port :: Port } "port: 0\n" of
        Left _ -> fail "should parse"
        Right c -> c.port `shouldEqual` Port 0

    it "accepts port 65535" do
      case readYAML @{ port :: Port } "port: 65535\n" of
        Left _ -> fail "should parse"
        Right c -> c.port `shouldEqual` Port 65535

    it "rejects negative port" do
      readYAML @{ port :: Port } "port: -1\n" `shouldSatisfy` isLeft

    it "rejects port above 65535" do
      readYAML @{ port :: Port } "port: 65536\n" `shouldSatisfy` isLeft

  describe "Secret" do
    it "parses and masks in show" do
      case readYAML @{ password :: Secret String } "password: hunter2\n" of
        Left _ -> fail "should parse"
        Right c -> do
          revealSecret c.password `shouldEqual` "hunter2"
          show c.password `shouldEqual` "Secret(***)"

    it "works with nested types" do
      case readYAML @{ token :: Secret Int } "token: 42\n" of
        Left _ -> fail "should parse"
        Right c -> revealSecret c.token `shouldEqual` 42

  describe "NonBlank" do
    it "parses non-blank string" do
      case readYAML @{ name :: NonBlank } "name: hello\n" of
        Left _ -> fail "should parse"
        Right c -> unNonBlank c.name `shouldEqual` "hello"

    it "trims whitespace" do
      case readYAML @{ name :: NonBlank } "name: \"  hello  \"\n" of
        Left _ -> fail "should parse"
        Right c -> unNonBlank c.name `shouldEqual` "hello"

    it "rejects empty string" do
      readYAML @{ name :: NonBlank } "name: \"\"\n" `shouldSatisfy` isLeft

    it "rejects whitespace-only string" do
      readYAML @{ name :: NonBlank } "name: \"   \"\n" `shouldSatisfy` isLeft

    it "mkNonBlank rejects empty string" do
      mkNonBlank "" `shouldEqual` Nothing

    it "mkNonBlank rejects whitespace" do
      mkNonBlank "   " `shouldEqual` Nothing

    it "mkNonBlank trims and returns value" do
      case mkNonBlank "  hello  " of
        Nothing -> fail "should succeed"
        Just nb -> unNonBlank nb `shouldEqual` "hello"

    it "rejects tab-only via YAML" do
      readYAML @{ name :: NonBlank } "name: \"\\t\"\n" `shouldSatisfy` isLeft

  describe "fromProfileDir" do
    it "loads default profile when env not set" do
      liftEffect $ Process.unsetEnv "TEST_PROFILE"
      let source = fromProfileDir { dir: "test/fixtures/config", ext: ".yaml", envVar: "TEST_PROFILE", default: "development" }
      result <- loadConfig @{ host :: String, port :: Int } source
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 3000

    it "overlays profile-specific config" do
      liftEffect $ Process.setEnv "TEST_PROFILE" "production"
      let source = fromProfileDir { dir: "test/fixtures/config", ext: ".yaml", envVar: "TEST_PROFILE", default: "development" }
      result <- loadConfig @{ host :: String, port :: Int } source
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "prod.example.com"
          config.port `shouldEqual` 443

  describe "real-world usage" do
    it "loads typed config from YAML with env resolution" do
      liftEffect do
        Process.setEnv "DB_HOST" "db.prod.internal"
        Process.setEnv "DB_PASSWORD" "s3cret"
      result <- loadConfig @FullConfig (fromYAMLString appYaml # withEnvResolution)
      case result of
        Left errs -> fail ("should parse but got: " <> show errs)
        Right config -> do
          unNonBlank config.server.host `shouldEqual` "localhost"
          config.server.port `shouldEqual` Port 8080
          config.server.timeout `shouldEqual` Duration (Milliseconds 30000.0)
          config.server.maxUpload `shouldEqual` Bytes 10485760.0
          config.database.host `shouldEqual` "db.prod.internal"
          config.database.port `shouldEqual` 5432
          unNonBlank config.database.name `shouldEqual` "myapp"
          (fromMaybe "" (revealSecret <$> config.database.password)) `shouldEqual` "s3cret"
          config.database.pool `shouldEqual` 10
          unNonBlank config.app.name `shouldEqual` "MyApp"
          config.app.debug `shouldEqual` true

    it "overrides defaults with env-specific config" do
      let source = override (fromRecord { host: "localhost", port: 3000, debug: false }) (fromYAMLString "port: 443\ndebug: true\n")
      result <- loadConfig @{ host :: String, port :: Int, debug :: Boolean } source
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 443
          config.debug `shouldEqual` true

    it "composes multiple sources: defaults, file, env" do
      liftEffect $ Process.setEnv "TESTAPP_HOST" "env-override"
      let defaults = fromRecord { host: "localhost", port: 3000 }
      let fileConfig = fromJSONString """{"port": 8080}"""
      let envConfig = fromEnvPrefix "TESTAPP"
      result <- loadConfig @{ host :: String, port :: Int } (override (override defaults fileConfig) envConfig)
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "env-override"
          config.port `shouldEqual` 8080

  describe "WriteForeign roundtrip" do
    it "Duration roundtrips through serialization" do
      let original = Duration (Milliseconds 30000.0)
      result <- loadConfig @{ timeout :: Duration } (fromRecord { timeout: original })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` original

    it "Duration serializes as human-readable" do
      result <- loadConfig @{ timeout :: String } (fromRecord { timeout: Duration (Milliseconds 30000.0) })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` "30s"

    it "Bytes roundtrips through serialization" do
      let original = Bytes 5242880.0
      result <- loadConfig @{ size :: Bytes } (fromRecord { size: original })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` original

    it "Bytes serializes as human-readable" do
      result <- loadConfig @{ size :: String } (fromRecord { size: Bytes 5242880.0 })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` "5MB"

    it "Duration sub-second stays as ms" do
      result <- loadConfig @{ timeout :: String } (fromRecord { timeout: Duration (Milliseconds 750.0) })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.timeout `shouldEqual` "750ms"

    it "Bytes sub-KB stays as B" do
      result <- loadConfig @{ size :: String } (fromRecord { size: Bytes 512.0 })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.size `shouldEqual` "512B"

    it "Secret roundtrips" do
      result <- loadConfig @{ password :: Secret String } (fromRecord { password: mkSecret "hunter2" })
      case result of
        Left _ -> fail "should parse"
        Right c -> revealSecret c.password `shouldEqual` "hunter2"

    it "Port roundtrips" do
      result <- loadConfig @{ port :: Port } (fromRecord { port: Port 8080 })
      case result of
        Left _ -> fail "should parse"
        Right c -> c.port `shouldEqual` Port 8080

    it "NonBlank roundtrips" do
      case mkNonBlank "hello" of
        Nothing -> fail "should create"
        Just nb -> do
          result <- loadConfig @{ name :: NonBlank } (fromRecord { name: nb })
          case result of
            Left _ -> fail "should parse"
            Right c -> unNonBlank c.name `shouldEqual` "hello"

  describe "ConfigSource Semigroup" do
    it "combines sources with <>" do
      result <- loadConfig @{ host :: String, port :: Int } (fromRecord { host: "localhost", port: 3000 } <> fromJSONString """{"port": 8080}""")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 8080

    it "mempty acts as identity" do
      result <- loadConfig @{ host :: String, port :: Int } (mempty <> fromRecord { host: "test", port: 42 })
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "test"
          config.port `shouldEqual` 42

  describe "fromFile and fromFileOptional" do
    it "fromFile detects YAML extension" do
      result <- loadConfig @{ server :: { host :: String } } (fromFile "test/fixtures/config/app.yaml")
      case result of
        Left _ -> fail "should parse"
        Right config -> config.server.host `shouldEqual` "localhost"

    it "fromFileOptional returns empty for missing file" do
      result <- loadConfig @{ host :: String, port :: Int } (override (fromRecord { host: "default", port: 3000 }) (fromFileOptional "nonexistent.yaml"))
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.host `shouldEqual` "default"
          config.port `shouldEqual` 3000

    it "fromFile detects TOML extension" do
      result <- loadConfig @{ server :: { host :: String, port :: Int } } (fromFile "test/fixtures/config/app.toml")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.server.host `shouldEqual` "localhost"
          config.server.port `shouldEqual` 8080

    it "fromFile detects JSON extension" do
      result <- loadConfig @{ server :: { host :: String, port :: Int } } (fromFile "test/fixtures/config/app.json")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.server.host `shouldEqual` "localhost"
          config.server.port `shouldEqual` 8080

    it "fromTOMLFile loads TOML" do
      result <- loadConfig @{ server :: { host :: String }, database :: { name :: String } } (fromTOMLFile "test/fixtures/config/app.toml")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.server.host `shouldEqual` "localhost"
          config.database.name `shouldEqual` "mydb"

    it "fromJSONFile loads JSON" do
      result <- loadConfig @{ server :: { host :: String }, database :: { name :: String } } (fromJSONFile "test/fixtures/config/app.json")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.server.host `shouldEqual` "localhost"
          config.database.name `shouldEqual` "mydb"

    it "fromFile rejects unknown extension" do
      result <- Aff.try (loadConfig @{ host :: String } (fromFile "config.xyz"))
      case result of
        Right _ -> fail "should throw"
        Left err -> message err `shouldSatisfy` \msg -> String.contains (Pattern "Unknown config file extension") msg

  describe "env prefix type coercion" do
    it "coerces numeric env values to numbers" do
      liftEffect $ Process.setEnv "COERCE_PORT" "8080"
      result <- loadConfig @{ port :: Int } (fromEnvPrefix "COERCE")
      case result of
        Left _ -> fail "should parse"
        Right config -> config.port `shouldEqual` 8080

    it "coerces boolean env values" do
      liftEffect do
        Process.setEnv "COERCE_DEBUG" "true"
        Process.setEnv "COERCE_VERBOSE" "false"
      result <- loadConfig @{ debug :: Boolean, verbose :: Boolean } (fromEnvPrefix "COERCE")
      case result of
        Left _ -> fail "should parse"
        Right config -> do
          config.debug `shouldEqual` true
          config.verbose `shouldEqual` false

    it "keeps plain strings as strings" do
      liftEffect $ Process.setEnv "COERCE_NAME" "myapp"
      result <- loadConfig @{ name :: String } (fromEnvPrefix "COERCE")
      case result of
        Left _ -> fail "should parse"
        Right config -> config.name `shouldEqual` "myapp"

  describe "mkSecret" do
    it "wraps a value" do
      revealSecret (mkSecret "password123") `shouldEqual` "password123"

    it "masks in show" do
      show (mkSecret 42) `shouldEqual` "Secret(***)"

  describe "OmLayer" do
    it "configLayer provides typed config" do
      result <- Om.runOm {}
        { exception: \(_ :: Error) -> pure Nothing
        , configParseError: \_ -> pure Nothing
        }
        do
          { config } <- runLayer {} (configLayer @{ host :: String, port :: Int } (fromRecord { host: "localhost", port: 3000 }))
          pure (Just config)
      case result of
        Nothing -> fail "should succeed"
        Just config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 3000

    it "configLayer reports parse errors" do
      gotError <- Om.runOm {}
        { exception: \(_ :: Error) -> pure false
        , configParseError: \_ -> pure true
        }
        do
          _ <- runLayer {} (configLayer @{ host :: String, port :: Int } (fromRecord { wrong: true }))
          pure false
      gotError `shouldEqual` true

    it "configLayerFromSources merges and provides" do
      let sources =
            [ fromRecord { host: "localhost", port: 3000 }
            , fromJSONString """{"port": 8080}"""
            ]
      result <- Om.runOm {}
        { exception: \(_ :: Error) -> pure Nothing
        , configParseError: \_ -> pure Nothing
        }
        do
          { config } <- runLayer {} (configLayerFromSources @{ host :: String, port :: Int } sources)
          pure (Just config)
      case result of
        Nothing -> fail "should succeed"
        Just config -> do
          config.host `shouldEqual` "localhost"
          config.port `shouldEqual` 8080

    it "configLayer' provides literal config" do
      result <- Om.runOm {}
        { exception: \(_ :: Error) -> pure Nothing }
        do
          { config } <- runLayer {} (configLayer' { host: "test", port: 42 })
          pure (Just config)
      case result of
        Nothing -> fail "should succeed"
        Just config -> do
          config.host `shouldEqual` "test"
          config.port `shouldEqual` 42

  describe "validation utils" do
    it "validateYAML returns Right for valid config" do
      case validateYAML @AppConfig "host: localhost\nport: 8080\ndatabase:\n  name: mydb\n  pool: 10\n" of
        Left err -> fail err
        Right _ -> pure unit

    it "validateYAML returns Left with readable error for invalid config" do
      case validateYAML @AppConfig "host: localhost\nport: not_a_number\n" of
        Right _ -> fail "should fail"
        Left err -> err `shouldSatisfy` \e -> String.contains (Pattern "property") e

    it "validateTOML returns Right for valid config" do
      case validateTOML @{ host :: String, port :: Int } "host = \"localhost\"\nport = 8080\n" of
        Left err -> fail err
        Right _ -> pure unit

    it "validateConfig returns Right for valid source" do
      result <- validateConfig @AppConfig (fromRecord { host: "localhost", port: 3000, database: { name: "db", pool: 5 } })
      case result of
        Left err -> fail err
        Right _ -> pure unit

    it "validateConfig returns Left with readable error for invalid source" do
      result <- validateConfig @AppConfig (fromRecord { wrong: true })
      case result of
        Right _ -> fail "should fail"
        Left err -> err `shouldSatisfy` \e -> String.contains (Pattern "property") e

    it "validateJSON returns Right for valid config" do
      case validateJSON @{ host :: String, port :: Int } """{"host": "localhost", "port": 8080}""" of
        Left err -> fail err
        Right _ -> pure unit

    it "validateJSON returns Left for invalid config" do
      case validateJSON @{ host :: String, port :: Int } """{"host": 123}""" of
        Right _ -> fail "should fail"
        Left err -> err `shouldSatisfy` \e -> String.contains (Pattern "property") e

    it "validateConfig works with composed sources" do
      let source = fromRecord { host: "localhost", port: 3000 }
            <> fromYAMLString "timeout: \"30s\"\nmaxUpload: \"10MB\"\n"
      result <- validateConfig @{ host :: String, port :: Int, timeout :: Duration, maxUpload :: Bytes } source
      case result of
        Left err -> fail ("should pass but got: " <> err)
        Right config -> do
          config.host `shouldEqual` "localhost"
          config.timeout `shouldEqual` Duration (Milliseconds 30000.0)

appYaml :: String
appYaml = """server:
  host: localhost
  port: 8080
  timeout: "30s"
  maxUpload: "10MB"
database:
  host:
    env: DB_HOST
    default: localhost
  port: 5432
  name: myapp
  password:
    env: DB_PASSWORD
  pool: 10
app:
  name: MyApp
  debug: true
"""
