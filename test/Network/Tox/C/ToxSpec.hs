{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.C.ToxSpec where

import           Control.Applicative               ((<$>), (<*>))
import qualified Crypto.Saltine.Internal.ByteSizes as Sodium (boxPK, boxSK)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Arbitrary         (fromABS)
import           Data.Default.Class                (def)
import           Data.Proxy                        (Proxy (..))
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary         (arbitraryBoundedEnum)

import qualified Network.Tox.C                     as C


instance Arbitrary C.ProxyType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary C.SavedataType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary C.Options where
  arbitrary = C.Options
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitraryCString
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (fromABS <$> arbitrary)
    where
      arbitraryCString = filter (/= '\NUL') <$> arbitrary


getRight :: (Monad m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


spec :: Spec
spec = do
  describe "tox_version_is_compatible" $
    it "is compatible with the major/minor/patch of the linked library" $
      C.tox_version_is_compatible
        C.tox_version_major
        C.tox_version_minor
        C.tox_version_patch
      `shouldBe` True

  describe "Constants" $
    it "has constants equal to the hstox expected key size" $ do
      fromIntegral C.tox_public_key_size `shouldBe` Sodium.boxPK
      fromIntegral C.tox_secret_key_size `shouldBe` Sodium.boxSK
      C.tox_address_size `shouldBe` C.tox_public_key_size + 6
      C.tox_max_name_length `shouldBe` 128
      C.tox_max_status_message_length `shouldBe` 1007
      C.tox_max_friend_request_length `shouldBe` 1016
      C.tox_max_message_length `shouldBe` C.tox_max_custom_packet_size - 1
      C.tox_max_custom_packet_size `shouldBe` 1373
      C.tox_max_filename_length `shouldBe` 255
      C.tox_hash_length `shouldBe` C.tox_file_id_length

  describe "Options" $ do
    it "can be marshalled to C and back" $
      property $ \options -> do
        res <- C.withOptions options C.peekToxOptions
        res `shouldBe` Right options

    it "is saved correctly by pokeToxOptions" $
      property $ \options0 options1 -> do
        res <- C.withOptions options0 $ \ptr -> do
          C.pokeToxOptions options1 ptr (return ())
          C.peekToxOptions ptr
        res `shouldBe` Right options0

    it "has a 'def' that is equivalent to the C default options" $ do
      res <- C.withToxOptions C.peekToxOptions
      res `shouldBe` Right def

  describe "nospam" $
    it "can be retrieved after being set" $
      property $ \nospam ->
        must $ C.withDefaultTox $ \tox -> do
          C.tox_self_set_nospam tox nospam
          nospam' <- C.tox_self_get_nospam tox
          nospam' `shouldBe` nospam

  describe "public key" $ do
    it "is a prefix of the address" $
      must $ C.withDefaultTox $ \tox -> do
        pk <- C.toxSelfGetPublicKey tox
        addr <- C.toxSelfGetAddress tox
        BS.unpack addr `shouldStartWith` BS.unpack pk

    it "is not equal to the secret key" $
      must $ C.withDefaultTox $ \tox -> do
        pk <- C.toxSelfGetPublicKey tox
        sk <- C.toxSelfGetSecretKey tox
        pk `shouldNotBe` sk
