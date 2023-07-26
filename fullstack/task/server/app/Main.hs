module Main (main) where

import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Middleware.Cors qualified as Wai
import Protolude
import Server

main :: IO ()
main = do
  let port = 4000
  putStrLn @Text $ "Listening on port " <> show port
  Wai.run port $ Wai.simpleCors app
