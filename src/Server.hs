{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}
{-# Language TemplateHaskell #-}

module Server where

import Env (getGridState, getGridSpec)

import Servant ( Post
               , JSON
               , ServerT
               , hoistServer
               , ReqBody
               , Proxy(..)
               , type (:>)      -- Syntax for importing type operator
               , type (:<|>)
               , (:<|>)(..)
               )
import Servant.Server (Handler, Application, serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static (static)
import Control.Monad.Reader
import System.Log.FastLogger
import qualified Data.ByteString as T

data EnvStateReq = EnvStateReq { gridId :: Int, time :: Int } deriving ()
data EnvState = EnvState
data EnvSpecReq = EnvSpecReq { envId :: Int }
data EnvSpec = EnvSpec

type MGAppType = ReaderT (FastLogger, IO ()) IO

type MGApp
  = "State" :> (ReqBody '[JSON] EnvStateReq) :> (Post '[JSON] EnvState)
  :<|> "Spec" :> (ReqBody '[JSON] EnvSpecReq) :> (Post '[JSON] EnvSpec)
  :<|> "error" :> ReqBody '[JSON] String :> Post '[JSON] String
  :<|> "success" :> ReqBody '[JSON] String :> Post '[JSON] String

{--
appHandlers :: ServerT MGApp MGAppType
appHandlers
  = getGridState
  :<|> getGridSpec


writeError :: String -> MGAppType String
writeError s = do
  (logger, _) <- ask
  liftIO $ do
    logger $ toLogStr' ("Error:" ++ ":" ++ s ++ "\n")
    pure s

writeSuccess :: String -> MGAppType String
writeSuccess s = do
  (logger, _) <- ask
  liftIO $ do
    logger $ toLogStr' ("Success:"++ ":" ++ s ++ "\n")
    pure s

toLogStr' :: String -> LogStr
toLogStr' = toLogStr

handlerServer :: (FastLogger, IO ()) -> ServerT MGApp Handler
handlerServer fl = hoistServer (Proxy :: Proxy MGApp) readerToHandler appHandlers
  where
    readerToHandler :: MGAppType x -> Handler x
    readerToHandler r = liftIO $ runReaderT r fl

app :: (FastLogger, IO ()) -> Application
app fl = serve (Proxy :: Proxy MGApp) $ handlerServer fl

server :: IO ()
server = do
  putStrLn $ "Generated elm source size: "
  fl@(f, _) <- newFastLogger (LogFileNoRotate "test-log.txt" 512)
  f $ toLogStr ("\n----------------------\n"::String)
  f $ toLogStr ("\n"::String)
  run 4000 $ static $ app fl
--}
