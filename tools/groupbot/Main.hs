{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Main (main) where

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Exception       (AsyncException (UserInterrupt), catch,
                                          throwIO)
import           Control.Monad           (forever)
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Char8   as BS
import           Data.String             (fromString)
import           Data.Word               (Word32)
import           Foreign.Storable        (Storable (..))
import           System.Directory        (doesFileExist)
import           System.Exit             (exitSuccess)

import qualified Network.Tox.C           as C


bootstrapKey :: BS.ByteString
bootstrapKey =
  fst . Base16.decode . fromString $
    "F404ABAA1C99A9D37D61AB54898F56793E1DEF8BD46B1038B9D822E8460FAB67"

isMasterKey :: BS.ByteString -> Bool
isMasterKey key =
  (key ==) . fst . Base16.decode . fromString $
    "040F75B5C8995F9525F9A8692A6C355286BBD3CF248C984560733421274F0365"

botName :: String
botName = "groupbot"

bootstrapHost :: String
bootstrapHost = "biribiri.org"

savedataFilename :: String
savedataFilename = "groupbot.tox"

options :: BS.ByteString -> C.Options
options savedata = C.Options
  { C.ipv6Enabled  = True
  , C.udpEnabled   = True
  , C.proxyType    = C.ProxyTypeNone
  , C.proxyHost    = ""
  , C.proxyPort    = 0
  , C.startPort    = 33445
  , C.endPort      = 33545
  , C.tcpPort      = 3128
  , C.savedataType = if savedata == BS.empty then C.SavedataTypeNone else C.SavedataTypeToxSave
  , C.savedataData = savedata
  }


getRight :: (Monad m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


newtype UserData = UserData Word32
  deriving (Eq, Storable, Read, Show)

instance C.CHandler UserData where
  cSelfConnectionStatus _ conn ud = do
    putStrLn "SelfConnectionStatusCb"
    print conn
    return $ ud

  cFriendRequest tox pk msg ud = do
    putStrLn "FriendRequestCb"
    Right fn <- C.toxFriendAddNorequest tox pk
    putStrLn $ (BS.unpack . Base16.encode) pk
    putStrLn msg
    print fn
    return ud

  cFriendConnectionStatus tox fn status ud@(UserData gn) = do
    putStrLn "FriendConnectionStatusCb"
    print fn
    print status
    if status /= C.ConnectionNone
    then do
      putStrLn "Inviting!"
      _ <- C.toxConferenceInvite tox fn gn
      return ()
    else
      putStrLn "Friend offline"
    return ud

  cFriendMessage tox fn msgType msg ud = do
    putStrLn "FriendMessage"
    print fn
    print msgType
    putStrLn msg
    _ <- C.toxFriendSendMessage tox fn msgType msg
    return ud

  cConferenceInvite tox fn _confType cookie ud = do
    putStrLn "ConferenceInvite"
    print fn
    pk <- getRight =<< (C.toxFriendGetPublicKey tox fn)
    if isMasterKey pk
    then do
      putStrLn "Joining!"
      gn <- getRight =<< C.toxConferenceJoin tox fn cookie
      return $ UserData gn
    else do
      putStrLn "Not master!"
      return ud


loop :: MVar ud -> C.Tox ud -> IO a
loop ud tox =
  forever $ do
    C.toxIterate tox ud
    interval <- C.tox_iteration_interval tox
    threadDelay $ fromIntegral $ interval * 10000


main :: IO ()
main = do
  exists <- doesFileExist savedataFilename
  loadedSavedata <- if exists then BS.readFile savedataFilename else return BS.empty
  must $ C.withOptions (options loadedSavedata) $ \optPtr ->
    must $ C.withTox optPtr $ \tox -> do
      must $ C.toxBootstrap tox bootstrapHost 33445 bootstrapKey

      C.withCHandler tox $ do
        adr <- C.toxSelfGetAddress tox
        putStrLn $ (BS.unpack . Base16.encode) adr
        _ <- C.toxSelfSetName tox botName
        gn <- getRight =<< C.toxConferenceNew tox
        ud <- newMVar (UserData gn)
        catch (loop ud tox) $ \case
          e@UserInterrupt -> throwIO e
          _ -> do
            savedSavedata <- C.toxGetSavedata tox
            BS.writeFile savedataFilename savedSavedata
            exitSuccess
