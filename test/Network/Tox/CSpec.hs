{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.CSpec where

import           Control.Applicative     ((<$>))
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (replicateM_, when)
import qualified Crypto.Saltine.Class    as Sodium (decode, encode)
import           Crypto.Saltine.Core.Box (PublicKey)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import           Data.String             (fromString)
import           Foreign.Marshal.Alloc   (alloca)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (freeHaskellFunPtr, nullPtr)
import           Foreign.Storable        (Storable (..))
import           Test.Hspec

import qualified Network.Tox.C           as C


bootstrapKey :: PublicKey
Just bootstrapKey =
  Sodium.decode . fst . Base16.decode . fromString $
    "F404ABAA1C99A9D37D61AB54898F56793E1DEF8BD46B1038B9D822E8460FAB67"

bootstrapHost :: String
bootstrapHost = "biribiri.org"


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


getRight :: (Monad m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


newtype UserData = UserData Int
  deriving (Eq, Storable)

instance C.CHandler UserData where
  cSelfConnectionStatus _ conn ud = do
    print conn
    poke ud (UserData 4321)


spec :: Spec
spec =
  describe "toxcore" $
    it "can bootstrap" $
      (getRight =<<) $ C.withOptions options $ \optPtr ->
        (getRight =<<) $ C.withTox optPtr $ \tox -> do
          getRight =<< C.toxBootstrap   tox bootstrapHost 33445 bootstrapKey
          getRight =<< C.toxAddTcpRelay tox bootstrapHost 33445 bootstrapKey

          print =<< C.tox_self_get_nospam tox

          C.withHandler tox $
            with (UserData 1234) $ \userData ->
              while ((/= UserData 4321) <$> peek userData) $ do
                putStrLn "tox_iterate"
                C.tox_iterate tox userData
                interval <- C.tox_iteration_interval tox
                threadDelay $ fromIntegral $ interval * 10000
