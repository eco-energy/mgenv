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

import Grid (getGridState, getGridSpec)

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

type MGAppType = ReaderT (FastLogger, IO ()) IO

type MGApp
  = "State" :> (ReqBody '[JSON] GridStateReq) :> (Post '[JSON] GridState)
  :<|> "Spec" :> (ReqBody '[JSON] GridSpecReq) :> (Post '[JSON] GridSpec)
  :<|> "error" :> ReqBody '[JSON] String :> Post '[JSON] String
  :<|> "success" :> ReqBody '[JSON] String :> Post '[JSON] String

appHandlers :: ServerT MGApp MGAppType
appHandlers
  = getGridState
  :<|> getGridSpec


writeError :: String -> MGAppType String
writeError s = do
  (logger, _) <- ask
  liftIO $ do
    logger $ toLogStr' ("Error:" ++ idxString ++ ":" ++ s ++ "\n")
    pure s

writeSuccess :: String -> MGAppType String
writeSuccess s = do
  (logger, _) <- ask
  liftIO $ do
    logger $ toLogStr' ("Success:"++ idxString ++ ":" ++ s ++ "\n")
    pure s

toLogStr' :: String -> LogStr
toLogStr' = toLogStr

idxString :: String
idxString = show idx

handlerServer :: (FastLogger, IO ()) -> ServerT AppType Handler
handlerServer fl = hoistServer (Proxy :: Proxy AppType) readerToHandler appHandlers
  where
    readerToHandler :: MyAppType x -> Handler x
    readerToHandler r = liftIO $ runReaderT r fl

app :: (FastLogger, IO ()) -> Application
app fl = serve (Proxy :: Proxy AppType) $ handlerServer fl

server :: IO ()
server = do
  putStrLn $ "Testing Aeson options index : " ++ (show idx)
  putStrLn $ "Generated elm source size: " ++ (show $ T.length elmSource)
  fl@(f, _) <- newFastLogger (LogFileNoRotate "test-log.txt" 512)
  f $ toLogStr ("\n----------------------\n"::String)
  f $ toLogStr ("Starting "++ show idx ++ "...\n")
  f $ toLogStr' $ show defaultOptions
  f $ toLogStr ("\n"::String)
  run 4000 $ static $ app fl
