module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import GraphQL (class IsQuery, N(..), genericQuery, render, (==>))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype RedisHGetAll
  = RedisHGetAll { key :: String }

instance redisHGetAllQuery :: IsQuery RedisHGetAll ( field :: String, value :: String ) where
  renderQuery (RedisHGetAll rec) = genericQuery "redisHGetAll" rec

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "query-spec" do
          it "should render a query" do
            let
              query =
                { keywords: RedisHGetAll { key: "keywords" } ==> { field: N :: N "key", value: N :: N "keyword" }
                , values: RedisHGetAll { key: "values" } ==> { value: N :: N "value" }
                }
            render query `shouldEqual` "query { keywords: redisHGetAll(key: \"keywords\", ){ key: field, keyword: value, }, values: redisHGetAll(key: \"values\", ){ value: value, }, }"
