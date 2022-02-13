{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StrictData                 #-}
module Main (main) where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (AsyncException (UserInterrupt), catch,
                                         throwIO)
import           Control.Monad          (foldM)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS
import           Data.String            (fromString)
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.IO           as Text
import           Data.Word              (Word32)
import           Foreign.Storable       (Storable (..))
import           System.Directory       (doesFileExist)
import           System.Exit            (exitSuccess)

import qualified Network.Tox.C          as C
import           Network.Tox.C.Events


bootstrapKey, masterKey :: BS.ByteString
Right bootstrapKey =
    Base16.decode . fromString $
        "3F0A45A268367C1BEA652F258C85F4A66DA76BCAA667A49E770BCC4917AB6A25"
Right masterKey =
    Base16.decode . fromString $
        "040F75B5C8995F9525F9A8692A6C355286BBD3CF248C984560733421274F0365"

isMasterKey :: BS.ByteString -> Bool
isMasterKey = (masterKey ==)

botName :: String
botName = "groupbot"

bootstrapHost :: String
bootstrapHost = "tox.initramfs.io"

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


getRight :: (MonadFail m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


newtype UserData = UserData { groupNumber :: Word32 }
    deriving (Eq, Storable, Read, Show)

handleEvent :: C.Tox a -> UserData -> Event -> IO UserData
handleEvent tox ud@(UserData gn) = \case
    SelfConnectionStatus conn -> do
        putStrLn "SelfConnectionStatusCb"
        print conn
        return ud

    FriendRequest (PublicKey pk) msg -> do
        putStrLn "FriendRequestCb"
        Right fn <- C.toxFriendAddNorequest tox pk
        putStrLn $ (BS.unpack . Base16.encode) pk
        Text.putStrLn $ Text.decodeUtf8 msg
        print fn
        return ud

    FriendConnectionStatus fn status -> do
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

    FriendMessage fn msgType msg -> do
        putStrLn "FriendMessage"
        print fn
        print msgType
        Text.putStrLn $ Text.decodeUtf8 msg
        _ <- C.toxFriendSendMessage tox fn msgType msg
        return ud

    ConferenceInvite fn _confType cookie -> do
        putStrLn "ConferenceInvite"
        print fn
        pk <- getRight =<< C.toxFriendGetPublicKey tox fn
        if isMasterKey pk
            then do
                putStrLn "Joining!"
                newGn <- getRight =<< C.toxConferenceJoin tox fn cookie
                return $ UserData newGn
            else do
                putStrLn "Not master!"
                return ud

    _ -> return ud


loop :: C.Tox a -> UserData -> IO ()
loop tox ud = do
    interval <- C.toxIterationInterval tox
    threadDelay $ fromIntegral $ interval * 10000
    events <- C.toxEventsIterate tox
    case events of
        Left err -> fail $ show err
        Right ok -> foldM (handleEvent tox) ud ok >>= loop tox


main :: IO ()
main = do
    exists <- doesFileExist savedataFilename
    loadedSavedata <- if exists then BS.readFile savedataFilename else return BS.empty
    must $ C.withOptions (options loadedSavedata) $ \optPtr ->
        must $ C.withTox optPtr $ \tox -> do
            must $ C.toxBootstrap tox bootstrapHost 33445 bootstrapKey

            adr <- C.toxSelfGetAddress tox
            putStrLn $ (BS.unpack . Base16.encode) adr
            _ <- C.toxSelfSetName tox botName
            gn <- getRight =<< C.toxConferenceNew tox
            catch (loop tox (UserData gn)) $ \case
                e@UserInterrupt -> throwIO e
                _ -> do
                    savedSavedata <- C.toxGetSavedata tox
                    BS.writeFile savedataFilename savedSavedata
                    exitSuccess
