{-# LANGUAGE NoImplicitPrelude #-}

module Data.Salak(
    Property(..)
  , Properties(..)
  , Key
  , Return(..)
  , insert
  , toKeys
  , empty
  , lookup
  , makePropertiesFromEnvironment
  , defaultParseCommandLine
  , makePropertiesFromCommandLine
  , ParseCommandLine
  , makePropertiesFromJson
  , makePropertiesFromYaml
  , defaultProperties
  , defaultProperties'
  , defaultPropertiesWithFile
  , FileName
  ) where

import           Data.Maybe
import           Data.Salak.Aeson
import           Data.Salak.CommandLine
import           Data.Salak.Environment
import           Data.Salak.Property
import           Data.Salak.Yaml
import           Prelude                hiding (empty, lookup)
import           System.Directory
import           System.FilePath        ((</>))

defaultProperties :: IO Properties
defaultProperties = defaultProperties' defaultParseCommandLine

defaultProperties' :: ParseCommandLine -> IO Properties
defaultProperties' dpc
  = makePropertiesFromCommandLine dpc empty
  >>= makePropertiesFromEnvironment

type FileName = String

defaultPropertiesWithFile :: FileName -> IO Properties
defaultPropertiesWithFile name = do
  p <- defaultProperties
  let n  = fromMaybe name $ (lookup "salak.config.name" p :: Maybe String)
      p' = insert (toKeys "salak.config.name") (PStr n) p
  c <- getCurrentDirectory
  h <- getHomeDirectory
  foldl (go n) (return p') $ [(lookup "salak.config.dir" p, False), (Just c, True), (Just h, True)]
  where
    go _    p (Nothing, _) = p
    go name p (Just d, ok) = let f = d </> name in do
      p' <- p
      b <- doesFileExist f
      if b
        then makePropertiesFromYaml f p'
        else if ok then return p' else error $ "File " ++ f ++  " not found"
