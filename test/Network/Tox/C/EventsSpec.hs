{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE ViewPatterns               #-}
module Network.Tox.C.EventsSpec where

import           Control.Concurrent     (threadDelay)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.List              (sort)
import           Data.MessagePack       (Object (..))
import qualified Data.MessagePack       as MP
import           Data.String            (fromString)
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.C          as C
import           Network.Tox.C.Events


bootstrapKey :: BS.ByteString
Right bootstrapKey =
  Base16.decode . fromString $
    "8E7D0B859922EF569298B4D261A8CCB5FEA14FB91ED412A7603A585A25698832"

bootstrapHost :: String
bootstrapHost = "95.79.50.56"


options :: C.Options
options = C.Options
  { C.ipv6Enabled  = True
  , C.udpEnabled   = True
  , C.proxyType    = C.ProxyTypeNone
  , C.proxyHost    = ""
  , C.proxyPort    = 0
  , C.startPort    = 33445
  , C.endPort      = 33545
  , C.tcpPort      = 3128
  , C.savedataType = C.SavedataTypeNone
  , C.savedataData = BS.empty
  }


getRight :: (MonadFail m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


processEvent :: Event -> IO Bool
processEvent SelfConnectionStatus{} = return True
processEvent _                      = return False


toxIterate :: C.Tox -> IO ()
toxIterate tox = do
    putStrLn "tox_iterate"
    interval <- C.toxIterationInterval tox
    threadDelay $ fromIntegral $ interval * 10000

    events <- must $ C.toxEventsIterate tox
    result <- mapM processEvent events

    if or result
       then putStrLn "connected"
       else toxIterate tox


spec :: Spec
spec = do
    describe "serialisation format" $ do
        it "is linear encoding" $ do
            MP.toObject MP.defaultConfig (FileChunkRequest 1 2 3 4)
                `shouldBe` ObjectArray
                    [ ObjectWord 11
                    , ObjectArray
                        [ObjectWord 1,ObjectWord 2,ObjectWord 3,ObjectWord 4]]

        it "can round-trip through toxcore" $ do
            property $ \(sort -> events) -> do
                events' <- C.toxEventsToPtr events >>= C.toxEventsFromPtr
                sort <$> events' `shouldBe` Right events

    describe "toxcore" $ do
        it "can bootstrap" $ do
            putStrLn "bootstrap"
            tox <- must $ C.toxNew options
            must $ C.toxBootstrap   tox bootstrapHost 33445 bootstrapKey
            must $ C.toxAddTcpRelay tox bootstrapHost 33445 bootstrapKey

            toxIterate tox
