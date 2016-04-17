{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Aeson
import Data.Aeson.Flatten
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)

decodeExample :: ByteString -> Value
decodeExample = fromJust . decodeStrict'

shouldResultIn :: ByteString -> ByteString -> Expectation
shouldResultIn input output = flatten (decodeExample input) `shouldBe` flatten (decodeExample output)

-- Test Cases

main :: IO ()
main = hspec $
  describe "Data.Aeson.Flatten" $
    describe "flatten" $ do
      it "works for empty object" $
        "{}" `shouldResultIn` "{}"
      it "works for simple object" $
        "{ \"a\": { \"b\": 1 } }" `shouldResultIn` "{ \"b\": 1 }"
      it "preserves values from more significant level in case of name clash" $
        "{ \"a\" : { \"b\": 1 }, \"b\": 2 }" `shouldResultIn` "{ \"b\": 2 }"
      it "works for one level flattening" $
        "{ \"a\": { \"b\": 1, \"c\": 2 } }" `shouldResultIn` "{ \"b\": 1, \"c\": 2 }"
      it "works for two level flattening" $
        "{ \"a\": { \"b\": { \"c\": 1 } } }" `shouldResultIn` "{ \"c\": 1 }"
      it "works for array with one level objects" $
        "{ \"a\": [ { \"b\": { \"c\": 1 } }, { \"b\": { \"c\": 234 } } ] }" `shouldResultIn` "{ \"a\": [ { \"c\": 1 }, { \"c\": 234 } ] }"
