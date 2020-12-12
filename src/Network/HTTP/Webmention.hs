{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Webmention
  ( webmention,
    notify,
    verify,
    Webmention,
    WebmentionNotification (..),
    WebmentionVerification (..),
    WebmentionError,
    SourceURI,
    TargetURI,
    EndpointURI,
    StatusURI,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive
import Data.Char
import Data.Either.Combinators
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Text.HTML.TagSoup
import Text.URI

type EndpointURI = URI

type StatusURI = URI

type SourceURI = URI

type TargetURI = URI

data Webmention = Webmention SourceURI TargetURI deriving (Eq, Show)

newtype WebmentionError = WebmentionError Text deriving (Eq, Show)

instance Exception WebmentionError where
  displayException (WebmentionError msg) = T.unpack msg

data WebmentionNotification
  = WebmentionAccepted
  | WebmentionPending StatusURI
  | WebmentionRejected EndpointURI (Response BL.ByteString)
  | WebmentionServerError EndpointURI (Response BL.ByteString)
  | WebmentionNotSupported
  deriving (Eq, Show)

data WebmentionVerification
  = SourceMentionsTarget
  | SourceMissingTarget
  | SourceServerError (Response BL.ByteString)
  deriving (Eq, Show)

-- | Construct a valid 'Webmention' from source and target 'URI'.
--
-- Throws a 'WebmentionError' if source and target are the same, are
-- relative, or lack an HTTP(S) scheme.
webmention :: MonadThrow m => SourceURI -> TargetURI -> m Webmention
webmention source target = do
  s <- isHttpUri source
  t <- isHttpUri target
  when (s == t) (throwM (WebmentionError "Source cannot match target."))
  return (Webmention s t)
  where
    isHttpUri :: MonadThrow m => URI -> m URI
    isHttpUri uri = do
      let scheme = unRText <$> uriScheme uri
      when (maybe False (\a -> "https" /= a && "http" /= a) scheme)
        $ throwM
        $ WebmentionError "Only HTTP and HTTPS URIs are allowed."
      when (isLeft (uriAuthority uri) || isNothing (uriScheme uri))
        $ throwM
        $ WebmentionError "URI must be absolute, including HTTP or HTTPS scheme and authority."
      return uri

-- | Notify target of 'Webmention'.
notify :: Webmention -> IO WebmentionNotification
notify wm@(Webmention source target) = do
  endpoint <- discoverEndpoint wm
  case endpoint of
    Nothing -> return WebmentionNotSupported
    Just uri -> do
      let body = BL.fromStrict (encodeUtf8 ("source=" <> render source <> "&target=" <> render target))
          length = encodeUtf8 (pack (show (BL.length body)))
          headers = [(hContentType, "application/x-www-form-urlencoded"), (hContentLength, length)]
      manager <- newTlsManagerWith $ tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 5000000}
      req' <- parseRequest (unpack (render uri))
      let req = req' {method = "POST", requestHeaders = headers, requestBody = RequestBodyLBS body}
      res <- httpLbs req manager
      return (wmNotification uri res)

-- | Verify 'Webmention' source mentions target.
verify :: Webmention -> IO WebmentionVerification
verify wm@(Webmention source target) = do
  manager <- newTlsManagerWith $ tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 5000000}
  request <- parseRequest (unpack (render source))
  res <- httpLbs request manager
  return (wmVerification wm res)

wmVerification :: Webmention -> Response BL.ByteString -> WebmentionVerification
wmVerification (Webmention _ target) res
  | status >= status200 && status < status300 =
    if encodeUtf8 (render target) `B.isInfixOf` (BL.toStrict (responseBody res))
      then SourceMentionsTarget
      else SourceMissingTarget
  | status >= status400 && status < status500 = SourceServerError res
  | otherwise = SourceServerError res
  where
    status = responseStatus res

wmNotification :: URI -> Response BL.ByteString -> WebmentionNotification
wmNotification uri res
  | status == status201 = maybe WebmentionAccepted WebmentionPending (parseLocationUrl (responseHeaders res))
  | status >= status200 && status < status300 = WebmentionAccepted
  | status >= status400 && status < status500 = WebmentionRejected uri res
  | otherwise = WebmentionServerError uri res
  where
    parseLocationUrl hs = mkURI . decodeUtf8 =<< (snd <$> L.find ((== hLocation) . fst) hs)
    status = responseStatus res

discoverEndpoint :: Webmention -> IO (Maybe URI)
discoverEndpoint (Webmention source target) = do
  manager <- newTlsManagerWith $ tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 5000000}
  request <- parseRequest (unpack (render target))
  (wm, res) <- withResponseHistory request manager $ \hr -> do
    let req = hrFinalRequest hr
        protocol = if secure req then "https://" else "http://"
        finalRes = hrFinalResponse hr
        finalUri = fromJust . mkURI . decodeUtf8 $ protocol <> host req <> path req <> queryString req
        redirectedWm = Webmention source finalUri -- to resolve endpoint against redirected URI
    body <- BL.fromChunks <$> brConsume (responseBody finalRes)
    return (redirectedWm, finalRes {responseBody = body})
  return (discoverEndpointFromHeader wm res <|> discoverEndpointFromHtml wm res)

discoverEndpointFromHeader :: Webmention -> Response BL.ByteString -> Maybe URI
discoverEndpointFromHeader (Webmention _ target) res = go (responseHeaders res)
  where
    go [] = Nothing
    go (h : hs) = matchWmHeader h <|> go hs
    headerVal txt = T.drop 1 (T.dropEnd 1 (L.head (strip <$> T.split (== ';') txt)))
    headerParams txt = L.drop 1 (strip <$> T.split (== ';') txt)
    headerVals (_, bs) = strip <$> split (== ',') (decodeUtf8 bs)
    isWmRel txt = T.take 4 txt == "rel=" && "webmention" `L.elem` T.splitOn " " (unquote (T.drop 4 txt))
    hasWmRel val = isJust (L.find isWmRel (headerParams val))
    isWmLink (k, _) = k == mk "Link"
    headerUri val = flip relativeTo target =<< mkURI val
    matchWmHeader h =
      if isWmLink h
        then headerUri =<< (headerVal <$> L.find hasWmRel (headerVals h))
        else Nothing
    unquote txt = if T.take 1 txt == "\"" then T.drop 1 (T.dropEnd 1 txt) else txt

discoverEndpointFromHtml :: Webmention -> Response BL.ByteString -> Maybe URI
discoverEndpointFromHtml (Webmention _ target) res =
  if isHtml (responseHeaders res) then go (parseTags (responseBody res)) else Nothing
  where
    go (t@(TagOpen "link" _) : ts) = if isRelWm t then hrefUri t <|> go ts else go ts
    go (t@(TagOpen "a" _) : ts) = if isRelWm t then hrefUri t <|> go ts else go ts
    go (t : ts) = go ts
    go [] = Nothing
    isHtml hs = maybe False (B.isInfixOf "html") (snd <$> L.find ((== hContentType) . fst) hs)
    hrefUri t = resolve =<< href t
    href (TagOpen _ attrs) = snd <$> L.find ((== "href") . fst) attrs
    resolve "" = Just target -- empty href should resolve to same page
    resolve bs = flip relativeTo target =<< mkURI (decodeUtf8 (BL.toStrict bs))
    isRelWm t = "webmention" `L.elem` T.splitOn " " (decodeUtf8 (BL.toStrict (fromAttrib "rel" t)))
