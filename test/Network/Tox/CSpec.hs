{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.CSpec where

import           Control.Applicative     ((<$>))
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Control.Exception       (bracket)
import           Control.Monad           (when)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import           Data.String             (fromString)
import           Foreign.StablePtr       (StablePtr, freeStablePtr,
                                          newStablePtr)
import           Foreign.Storable        (Storable (..))
import           Test.Hspec

import qualified Network.Tox.C           as C


bootstrapKey :: BS.ByteString
Right bootstrapKey =
  Base16.decode . fromString $
    "1C5293AEF2114717547B39DA8EA6F1E331E5E358B35F9B6B5F19317911C5F976"

bootstrapHost :: String
bootstrapHost = "tox.verdict.gg"


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


while :: IO Bool -> IO () -> IO ()
while cond io = do
  continue <- cond
  when continue $ io >> while cond io


getRight :: (MonadFail m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


newtype UserData = UserData Int
  deriving (Eq, Storable, Read, Show)

instance C.CHandler UserData where
  cSelfConnectionStatus _ conn ud = do
    print conn
    print ud
    return $ UserData 4321


withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr x = bracket (newStablePtr x) freeStablePtr


toxIterate :: MVar a -> C.Tox a -> IO ()
toxIterate ud tox =
  withStablePtr ud (C.tox_iterate tox)


spec :: Spec
spec =
  describe "toxcore" $
    it "can bootstrap" $
      must $ C.withOptions options $ \optPtr ->
        must $ C.withTox optPtr $ \tox -> do
          must $ C.toxBootstrap   tox bootstrapHost 33445 bootstrapKey
          must $ C.toxAddTcpRelay tox bootstrapHost 33445 bootstrapKey

          C.withCHandler tox $ do
            ud <- newMVar (UserData 1234)
            while ((/= UserData 4321) <$> readMVar ud) $ do
              toxIterate ud tox
              putStrLn "tox_iterate"
              interval <- C.toxIterationInterval tox
              threadDelay $ fromIntegral $ interval * 10000
            putStrLn "done"
