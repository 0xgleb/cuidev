{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad.Fail qualified as Fail
import Data.FileEmbed qualified as Embed
import Data.Morpheus qualified as Morpheus
import Data.Morpheus.Document qualified as Morpheus
import Data.Morpheus.Types qualified as Morpheus
import Network.HTTP.Client qualified as HTTP
import Network.Wai.Handler.Warp (run)
import Protolude
import Servant ((:>))
import Servant qualified
import Xmlbf qualified as Xml
import Xmlbf.Xeno qualified as Xml

Embed.makeRelativeToProject "schema.gql" >>= Morpheus.importGQLDocument

root :: Morpheus.RootResolver Servant.Handler () Query Morpheus.Undefined Morpheus.Undefined
root =
  Morpheus.RootResolver
    { queryResolver = Query {exchangeRates},
      mutationResolver = _,
      subscriptionResolver = _
    }
  where
    exchangeRates = do
      let url = "https://www.cnb.cz/cs/financni_trhy/devizovy_trh/kurzy_devizoveho_trhu/denni_kurz.xml"

      manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
      request <- liftIO $ HTTP.parseRequest url
      response <- liftIO $ _ <$> HTTP.httpLbs request manager

      let parseElement = do
            country <- Xml.pAttr "zeme"
            currency <- Xml.pAttr "mena"

            rate <- readMaybe @Double <$> Xml.pAttr "kurz" >>= maybe (Fail.fail "Invalid rate") pure
            per <- readMaybe @Double <$> Xml.pAttr "mnozstvi" >>= maybe (Fail.fail "Invalid rate") pure
            let amount = rate / per

            code <- Xml.pAttr "kod"

            pure $ ExchangeRate {..}

      let parser = do
            rows <- Xml.pElement "kurzy" $ Xml.pElement "tabulka" Xml.pChildren
            forM rows $ Xml.parse (Xml.pElement "radek" parseElement) . pure

      case Xml.fromRawXml response >>= Xml.parse parser of
        Left err -> _ $ show err
        Right parsed -> pure parsed

startApp :: IO ()
startApp = run 4000 app

type API =
  "graphql"
    :> Servant.ReqBody '[Servant.JSON] Morpheus.GQLRequest
    :> Servant.Post '[Servant.JSON] Morpheus.GQLResponse

api :: Servant.Proxy API
api = Servant.Proxy

server :: Servant.Server API
server = Morpheus.runApp $ Morpheus.deriveApp root

app :: Servant.Application
app = Servant.serve api server