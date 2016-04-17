{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: Data.Aeson.Prefix
-- Maintainer: Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- Hiearchical prefixing of JSON objects from 'Data.Aeson'
-- Please see examples for understanding what does it mean.
--
-- == Examples
--
-- usage of 'prefix'
--
-- === Basic
--
-- @{ "a": { "b": 1 } }@ results in @{ "a": { "a.b": 2 }@
--
-- @{ "a": { "b": { "c": 1 } } }@ results in @{ "a": { "a.b": { "a.b.c": 1 } } }@
--
-- === Arrays
--
-- @{ "a": [ { "b": 1 }, { "b": 2 } ] }@ is not changed
--
-- * Arrays don't inherit the prefix from their parent keys by default
--
-- === With 'optionPreservePrefix' set to 'True'
-- 
-- @{ "a": [ { "b": 1 }, { "b": 2 } ] }@ results in @{ "a": [ { "a.b": 1 }, { "a.b": 2 } ] }@
--
-- === With 'optionPrefix' set to "prefix"
--
-- @{ "a": 1 }@ results in @{ "prefix.a": 1 }@
--
-- * This affects only keys in top level object. 
-- * If array is top level, it doesn't take effect unless you set 'optionPreservePrefix' to 'True'
--
-- === With 'optionSeparator' set to "~"
--
-- @{ "a": { "b": 1 } }@ results in @{ "a": { "a~b": 2 } }@
module Data.Aeson.Prefix
    ( prefix
    -- * Options
    , Options(..)
    , Prefix
    , Separator
    , defaultOptions
    -- * Utility functions and types
    , Pair
    , prefixKey
    , prefixPair
    , withPrefix
    , withoutPrefix
    ) where
import Control.Monad.Reader (MonadReader, asks, local)

import Data.Aeson hiding (defaultOptions)
import qualified Data.HashMap.Strict as Map (fromList, toList)
import Data.Monoid ((<>))
import qualified Data.Vector as Vector (mapM)
import Data.Text (Text)
import qualified Data.Text as Text (singleton)

type Pair = (Text, Value)
type Prefix = Maybe Text
type Separator = Text

data Options = Options {
    -- |
    -- Preserve prefix in Arrays, objects in arrays preserve prefix from their parent key
    optionPreservePrefix :: Bool
    -- |
    -- Separator, text to delimit a prefix from a key
  , optionSeparator :: Separator
    -- |
    -- Prefix, prefix added to all keys of top level object
  , optionPrefix :: Prefix
  } deriving Show

-- |
-- Default options
-- 
-- * 'optionPreservePrefix' set to 'False'
-- * 'optionSeparator' set to "."
-- * 'optionPrefix' set to 'Nothing'
defaultOptions :: Options
defaultOptions = Options False (Text.singleton '.') Nothing

-- |
-- Change options to use supplied prefix
withPrefix :: Prefix -> Options -> Options
withPrefix p o = o { optionPrefix = p }

-- |
-- Change options to not use any prefix
withoutPrefix :: Options -> Options
withoutPrefix o = o { optionPrefix = Nothing }

-- |
-- Prefixes text with prefix if defined in options, a separator from options is used to delimit prefix and text
prefixKey :: forall m . (Monad m, MonadReader Options m) => Text -> m Text
prefixKey t = do
  sep <- asks optionSeparator
  pre <- asks optionPrefix
  return $ maybe t (\p -> p <> sep <> t) pre

-- |
-- Prefixes identifier (first in pair), this prefixed identifier is used as a prefix for value (second in pair)
prefixPair :: forall m . (Monad m, MonadReader Options m) => Pair -> m Pair
prefixPair (i, v) = do
  pk <- prefixKey i
  pv <- local (withPrefix $ Just pk) $ prefix v
  return (pk, pv)

-- |
-- Convert `Object` to list of `Pair`s
objectToPairs :: Object -> [Pair]
objectToPairs = Map.toList

-- Convert list of `Pair`s to `Object`
pairsToObject :: [Pair] -> Object
pairsToObject = Map.fromList

-- |
-- Prefixes supplied `Value` using `Options`
prefix :: forall m . (Monad m, MonadReader Options m) => Value -> m Value
prefix (Array a)  = do
  preserve <- asks optionPreservePrefix
  let prefixed = Array <$> Vector.mapM prefix a
  if preserve
    then prefixed
    else local withoutPrefix prefixed
prefix (Object o) = do
  p <- mapM prefixPair $ objectToPairs o
  return $ Object $ pairsToObject p
prefix v = return v
