module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Except (runExcept)
import Data.Array (length, (!!))
import Data.Either (Either(Right, Left))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, foldM, insert, keys)
import Data.String (drop)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Process (PROCESS, argv)
import Simple.JSON (class ReadForeign, class WriteForeign, read, readJSON, writeJSON)
import Text.Prettier (Parser(..), defaultOptions, format)

type Result =
  { name :: String
  , latest ::
      { dependencies :: StrMap String
      , version :: String -- probably needs v prefix applied after
      , repository ::
          { url :: String
          }
      }
  }

newtype PackageName = PackageName String
derive newtype instance rfpn :: ReadForeign PackageName
derive newtype instance wfpn :: WriteForeign PackageName
newtype GitUrl = GitUrl String
derive newtype instance rfgu :: ReadForeign GitUrl
derive newtype instance wfgu :: WriteForeign GitUrl
newtype Version = Version String
derive newtype instance rfv :: ReadForeign Version
derive newtype instance wfv :: WriteForeign Version

type PackageSpec =
  { version :: Version
  , repo :: GitUrl
  , dependencies :: Array PackageName
  }

type PackageSet = StrMap PackageSpec

foreign import _getLatest :: forall e
   . String
  -> (Error -> Eff e Unit)
  -> (Foreign -> Eff e Unit)
  -> Eff e Unit

getLatest :: forall e. String -> Aff e Foreign
getLatest name = makeAff $ _getLatest name

updatePackageSet :: PackageSet -> Aff _ PackageSet
updatePackageSet packageSet = do
  log $ "Processing " <> show (length (keys packageSet)) <> " items."
  log $ "This will take forever and will crash multiple times. Please see the progress in wip.json."
  log $ "If you know how to make bower not suck, please get in touch and contribute!"
  foldM go empty packageSet
  where
    go set key spec = do
      writeTextFile UTF8 "./wip.json" $ writeJSON set
      log $ "processing " <> key
      _result <- runExcept <<< read <$> getLatest ("purescript-" <> key)
      case _result of
        Right (result :: Result) -> do
          let newDependencies = PackageName <<< drop 11 <$> keys result.latest.dependencies
          let newSpec = spec {dependencies = newDependencies}
          pure $ insert key newSpec set
        Left e -> do
          error $ "couldn't fetch info for " <> key
          pure set


main :: forall e. Eff
  ( exception :: EXCEPTION
  , fs :: FS
  , console :: CONSOLE
  , process :: PROCESS
  | e
  )
  Unit
main = void $ launchAff do
  args <- liftEff $ argv
  case args !! 2 of
    Just path -> do
      _packageSet <- readJSON <$> readTextFile UTF8 path
      case _packageSet of
        Left e ->
          error $ "Error parsing package set: " <> show e
        Right packageSet -> do
          newPackageSet <- updatePackageSet packageSet
          writeTextFile UTF8 path $ format (defaultOptions {parser = JSON}) $ writeJSON newPackageSet
          log "Done"
    Nothing -> do
      error "No target package.json provided. Give one."
