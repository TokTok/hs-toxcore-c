{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.C.ToxSpec where

import qualified Data.ByteString as BS
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.C   as C

{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Redundant do" #-}

boxPK, boxSK :: Int
boxPK = 32
boxSK = 32

instance Arbitrary C.ProxyType where
    shrink = genericShrink
    arbitrary = arbitraryBoundedEnum

instance Arbitrary C.SavedataType where
    shrink = genericShrink
    arbitrary = arbitraryBoundedEnum

-- | Ensure that the hostname has a chance of being valid.
filterHost :: C.Options -> C.Options
filterHost o@C.Options{C.proxyHost = h} = o{C.proxyHost = filter (`elem` hostChars) h}
  where
    hostChars = ".-_" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

instance Arbitrary C.Options where
    shrink = map filterHost . genericShrink
    arbitrary = fmap filterHost $ C.Options
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary


getRight :: (MonadFail m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


spec :: Spec
spec = do
    describe "tox_version_is_compatible" $ do
        it "is compatible with the major/minor/patch of the linked library" $ do
            C.tox_version_is_compatible
                C.tox_version_major
                C.tox_version_minor
                C.tox_version_patch
                `shouldBe` True

    describe "Constants" $ do
        it "has constants equal to the hstox expected key size" $ do
            fromIntegral C.tox_public_key_size `shouldBe` boxPK
            fromIntegral C.tox_secret_key_size `shouldBe` boxSK
            C.tox_address_size `shouldBe` C.tox_public_key_size + 6
            C.tox_max_name_length `shouldBe` 128
            C.tox_max_status_message_length `shouldBe` 1007
            C.tox_max_friend_request_length `shouldBe` 1016
            C.tox_max_message_length `shouldBe` C.tox_max_custom_packet_size - 1
            C.tox_max_custom_packet_size `shouldBe` 1373
            C.tox_max_filename_length `shouldBe` 255
            C.tox_hash_length `shouldBe` C.tox_file_id_length

    describe "Options" $ do
        it "can be marshalled to C and back" $ do
            property $ \options -> do
                res <- C.withOptions options C.peekToxOptions
                res `shouldBe` Right options

        it "has a 'def' that is equivalent to the C default options" $ do
            res <- C.withToxOptions C.peekToxOptions
            res `shouldBe` Right C.defaultOptions

    describe "nospam" $ do
        it "can be retrieved after being set" $ do
            property $ \nospam ->
                must $ C.withTox C.defaultOptions $ \tox -> do
                    C.toxSelfSetNospam tox nospam
                    nospam' <- C.toxSelfGetNospam tox
                    nospam' `shouldBe` nospam

    describe "public key" $ do
        it "is a prefix of the address" $ do
            must $ C.withTox C.defaultOptions $ \tox -> do
                pk <- C.toxSelfGetPublicKey tox
                addr <- C.toxSelfGetAddress tox
                BS.unpack addr `shouldStartWith` BS.unpack pk

        it "is not equal to the secret key" $ do
            must $ C.withTox C.defaultOptions $ \tox -> do
                pk <- C.toxSelfGetPublicKey tox
                sk <- C.toxSelfGetSecretKey tox
                pk `shouldNotBe` sk

    describe "error code enum" $ do
        it "is correctly translated to Haskell" $ do
            must $ C.withTox C.defaultOptions $ \tox -> do
                err <- C.toxFriendSendMessage tox 0 C.MessageTypeNormal $ BS.pack [1, 2, 3]
                err `shouldBe` Left C.ErrFriendSendMessageFriendNotFound
