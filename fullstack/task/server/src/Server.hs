{-# LANGUAGE TemplateHaskell #-}

module Server (app) where

import Control.Monad.Fail ()
import Data.FileEmbed qualified as Embed
import Data.HashMap.Strict qualified as Map
import Data.Morpheus qualified as Morpheus
import Data.Morpheus.Document qualified as Morpheus
import Data.Morpheus.Types qualified as Morpheus
import Data.Text qualified as Txt
import Network.HTTP.Conduit qualified as Http
import Protolude
import Servant ((:>))
import Servant qualified
import Xmlbf qualified as Xml
import Xmlbf.Xeno qualified as Xml

Embed.makeRelativeToProject "schema.gql" >>= Morpheus.importGQLDocument

root :: Morpheus.RootResolver Servant.Handler () Query Morpheus.Undefined Morpheus.Undefined
root =
  Morpheus.defaultRootResolver {Morpheus.queryResolver = Query {exchangeRates}}
  where
    exchangeRates = do
      let url = "https://www.cnb.cz/cs/financni_trhy/devizovy_trh/kurzy_devizoveho_trhu/denni_kurz.xml"
      response <- liftIO $ toS <$> Http.simpleHttp url

      Right rates <- pure do
        nodes <- Xml.fromRawXml response

        Xml.parse (Xml.pElement "kurzy" $ Xml.pElement "tabulka" Xml.pChildren) nodes <&> mapMaybe \case
          Xml.Element _ attrs _ -> do
            country <- Map.lookup "zeme" attrs
            currency <- Map.lookup "mena" attrs
            code <- Map.lookup "kod" attrs

            per <- readMaybe @Integer =<< Map.lookup "mnozstvi" attrs
            rate <- readMaybe @Integer =<< Txt.filter (/= ',') <$> Map.lookup "kurz" attrs

            pure
              ExchangeRate
                { country = pure country,
                  currency = pure currency,
                  amount = pure $ fromInteger rate / (1000 * fromInteger per),
                  code = pure code
                }
          _ -> Nothing

      pure rates

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