{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Control.Monad.Reader (runReader)

import Data.Aeson
import Data.Aeson.Prefix
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)

defaultSeparator :: Separator
defaultSeparator = "."

defaultPrefix :: Text
defaultPrefix = "prefix"

optionsDefault :: Options
optionsDefault = Options False defaultSeparator Nothing

optionsPrefixArrays :: Options
optionsPrefixArrays = Options True defaultSeparator Nothing

optionsWithPrefix :: Options
optionsWithPrefix = Options False defaultSeparator (Just defaultPrefix)

optionsWithPrefixAndPrefixArrays :: Options
optionsWithPrefixAndPrefixArrays = Options True defaultSeparator (Just defaultPrefix)

decodeExample :: ByteString -> Value
decodeExample = fromJust . decodeStrict'

prefixTest :: Options -> ByteString -> ByteString -> Expectation
prefixTest opts input expected = (flip runReader opts . prefix) (decodeExample input) `shouldBe` decodeExample expected

allOptions :: [Options]
allOptions = [ optionsDefault
             , optionsPrefixArrays
             , optionsWithPrefix
             , optionsWithPrefixAndPrefixArrays
             ]

-- Test Inputs

inputSimple :: ByteString
inputSimple = "{ \"a\": 1 }"

inputOneLevel :: ByteString
inputOneLevel = "{ \"a\": { \"b\": 1, \"c\": 2 } }"

inputTwoLevel :: ByteString
inputTwoLevel = "{ \"a\": { \"b\": { \"d\": 1 }, \"c\": { \"e\": 2 } } }"

inputTopArray :: ByteString
inputTopArray = "[ { \"a\": { \"b\": 1 } }, { \"a\": { \"b\": 2 } } ]"

inputWithArrayOneLevel :: ByteString
inputWithArrayOneLevel = "{ \"a\": [ { \"b\": { \"c\": 1 } }, { \"b\": { \"c\": 2 } } ] }"

main :: IO ()
main = hspec $
  describe "Data.Aeson.Prefix" $
    describe "prefix" $ do
      it "doesn't change empty object" $
        mapM_ (\o -> prefixTest o "{}" "{}") allOptions
      it "doesn't change flat array" $ do
        let input = "[\"a\", \"b\"]"
        mapM_ (\o -> prefixTest o input input) allOptions
      describe "default options" $ do
        let shouldResultIn = prefixTest optionsDefault
        it "doesn't change simple object" $ 
          inputSimple `shouldResultIn` inputSimple
        it "works for one level prefixing" $ 
          inputOneLevel `shouldResultIn` "{ \"a\": { \"a.b\": 1, \"a.c\": 2 } }"
        it "works with two level prefixing" $ 
          inputTwoLevel `shouldResultIn` "{ \"a\": { \"a.b\": { \"a.b.d\": 1 }, \"a.c\": { \"a.c.e\": 2 } } }"
        it "works with top level array of one level prefixing" $
          inputTopArray `shouldResultIn` "[ { \"a\": { \"a.b\": 1 } }, { \"a\": { \"a.b\": 2 } } ]"
        it "works with array of one level prefixing" $ 
          inputWithArrayOneLevel `shouldResultIn` "{ \"a\": [ { \"b\": {\"b.c\": 1 } }, { \"b\": { \"b.c\": 2 } } ] }"
      describe "options with prefixArrays" $ do
        let shouldResultIn = prefixTest optionsPrefixArrays
        it "doesn't change simple object" $ 
          inputSimple `shouldResultIn` inputSimple
        it "works for one level prefixing" $ 
          inputOneLevel `shouldResultIn` "{ \"a\": { \"a.b\": 1, \"a.c\": 2 } }"
        it "works with two level prefixing" $ 
          inputTwoLevel `shouldResultIn` "{ \"a\": { \"a.b\": { \"a.b.d\": 1 }, \"a.c\": { \"a.c.e\": 2 } } }"
        it "works with top level array of one level prefixing" $
          inputTopArray `shouldResultIn` "[ { \"a\": { \"a.b\": 1 } }, { \"a\": { \"a.b\": 2 } } ]"
        it "works with array of one level prefixing" $ 
          inputWithArrayOneLevel `shouldResultIn` "{ \"a\": [ { \"a.b\": {\"a.b.c\": 1 } }, { \"a.b\": { \"a.b.c\": 2 } } ] }"
      describe "options with user defined prefix" $ do
        let shouldResultIn = prefixTest optionsWithPrefix
        it "doesn't change simple object" $ 
          inputSimple `shouldResultIn` "{ \"prefix.a\": 1 }"
        it "works for one level prefixing" $ 
          inputOneLevel `shouldResultIn` "{ \"prefix.a\": { \"prefix.a.b\": 1, \"prefix.a.c\": 2 } }"
        it "works with two level prefixing" $ 
          inputTwoLevel `shouldResultIn` "{ \"prefix.a\": { \"prefix.a.b\": { \"prefix.a.b.d\": 1 }, \"prefix.a.c\": { \"prefix.a.c.e\": 2 } } }"
        it "works with top level array of one level prefixing" $
          inputTopArray `shouldResultIn` "[ { \"a\": { \"a.b\": 1 } }, { \"a\": { \"a.b\": 2 } } ]"
        it "works with array of one level prefixing" $ 
          inputWithArrayOneLevel `shouldResultIn` "{ \"prefix.a\": [ { \"b\": {\"b.c\": 1 } }, { \"b\": { \"b.c\": 2 } } ] }"
      describe "options with prefixArrays and user defined prefix" $ do
        let shouldResultIn = prefixTest optionsWithPrefixAndPrefixArrays
        it "doesn't change simple object" $ 
          inputSimple `shouldResultIn` "{ \"prefix.a\": 1 }"
        it "works for one level prefixing" $ 
          inputOneLevel `shouldResultIn` "{ \"prefix.a\": { \"prefix.a.b\": 1, \"prefix.a.c\": 2 } }"
        it "works with two level prefixing" $ 
          inputTwoLevel `shouldResultIn` "{ \"prefix.a\": { \"prefix.a.b\": { \"prefix.a.b.d\": 1 }, \"prefix.a.c\": { \"prefix.a.c.e\": 2 } } }"
        it "works with top level array of one level prefixing" $
          inputTopArray `shouldResultIn` "[ { \"prefix.a\": { \"prefix.a.b\": 1 } }, { \"prefix.a\": { \"prefix.a.b\": 2 } } ]"
        it "works with array of one level prefixing" $ 
          inputWithArrayOneLevel `shouldResultIn` "{ \"prefix.a\": [ { \"prefix.a.b\": {\"prefix.a.b.c\": 1 } }, { \"prefix.a.b\": { \"prefix.a.b.c\": 2 } } ] }"
