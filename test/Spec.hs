{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Text
import Network.HTTP.Webmention
import Test.Hspec
import Text.URI

main :: IO ()
main = hspec $ do
  describe "Network.HTTP.Webmention" $ do
    it "HTTP Link header, unquoted rel, relative URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/1"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/1/webmention"
    it "HTTP Link header, unquoted rel, absolute URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/2"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/2/webmention"
    it "HTML <link> tag, relative URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/3"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/3/webmention"
    it "HTML <link> tag, absolute URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/4"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/4/webmention"
    it "HTML <a> tag, relative URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/5"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/5/webmention"
    it "HTML <a> tag, absolute URL" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/6"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/6/webmention"
    it "HTTP Link header with strange casing" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/8"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/8/webmention"
    it "HTTP Link header, quoted rel" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/8"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/8/webmention"
    it "Multiple rel values on a <link> tag" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/9"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/9/webmention"
    it "Multiple rel values on Link header" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/10"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/10/webmention"
    it "Multiple Webmention endpoints advertised: Link, <link>, <a>" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/11"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/11/webmention"
    it "Checking for exact match of rel=webmention" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/12"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/12/webmention"
    it "False endpoint inside an HTML comment" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/13"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/13/webmention"
    it "False endpoint in escaped HTML" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/14"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/14/webmention"
    it "Webmention href is an empty string" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/15"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/15"
    it "Multiple Webmention endpoints advertised: <a>, <link>" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/16"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/16/webmention"
    it "Multiple Webmention endpoints advertised: <link>, <a>" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/17"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/17/webmention"
    it "Multiple HTTP Link headers" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/18"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/18/webmention"
    it "Single HTTP Link header with multiple values" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/19"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/19/webmention"
    it "Link tag with no href attribute" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/20"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/20/webmention"
    it "Webmention endpoint has query string parameters" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/21"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/21/webmention?query=yes"
    it "Webmention endpoint is relative to the path" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/22"
      n <- notify =<< webmention source target
      endpoint n `shouldBe` Just "https://webmention.rocks/test/22/webmention"
    it "Webmention target is a redirect and the endpoint is relative" $ do
      source <- mkURI "https://webmention.rocks/"
      target <- mkURI "https://webmention.rocks/test/23/page"
      n <- notify =<< webmention source target
      endpoint n `shouldSatisfy` (maybe False (isPrefixOf "https://webmention.rocks/test/23/page/webmention-endpoint"))
    it "verifies webmention source mentions target" $ do
      source <- mkURI "https://webmention.rocks/test/6"
      target <- mkURI "https://webmention.rocks/test/6/webmention"
      v <- verify =<< webmention source target
      v `shouldBe` SourceMentionsTarget
    it "verifies webmention source does not mention target" $ do
      source <- mkURI "https://webmention.rocks/test/6"
      target <- mkURI "https://webmention.rocks/test/7"
      v <- verify =<< webmention source target
      v `shouldBe` SourceMissingTarget

endpoint :: WebmentionNotification -> Maybe Text
endpoint (WebmentionRejected endpoint _) = Just (render endpoint)
endpoint (WebmentionServerError endpoint _) = Just (render endpoint)
endpoint _ = Nothing
