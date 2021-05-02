module Test.Main where

import Prelude
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (launchAff_)
import GraphQL (class IsQuery, class IsRecordQuery, N(..), genericQuery, render, (==>))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype RedisHGetAll
  = RedisHGetAll { key :: String }

instance redisHGetAllQuery :: IsRecordQuery RedisHGetAll Array ( field :: String, value :: String ) where
  renderRecordQuery (RedisHGetAll rec) = genericQuery "redisHGetAll" rec

newtype RedisGet
  = RedisGet { key :: String }

instance redisGetQuery :: IsQuery RedisGet Identity String where
  renderQuery (RedisGet rec) = genericQuery "redisGet" rec

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "query-spec" do
          it "should render a query" do
            let
              query =
                { keywords: RedisHGetAll { key: "keywords" } ==> { key: N :: N "field", keyword: N :: N "value" }
                , value: RedisGet { key: "value" }
                }
            render query `shouldEqual` "query { keywords: redisHGetAll(key: \"keywords\", ){ key: field, keyword: value, }, value: redisGet(key: \"value\", ), }"
