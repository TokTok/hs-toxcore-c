{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Proxy               (Proxy (..))
import           Data.Schema.Builder      (Builder (..), ToSchema (..), atom)
import           Data.Schema.C            (genC)
--import           Data.Schema.Pretty   (ppSchema)
import           Data.Schema.Type         (Type (..))
import           Data.Word                (Word16, Word32, Word64)
import           GHC.TypeNats             (KnownNat, Nat, natVal)
--import           Text.Groom           (groom)

import           Network.Tox.C.Type
import           Network.Tox.Types.Events


instance KnownNat size => ToSchema (FixedByteString (size :: Nat)) where
    toSchema = atom (TyFixedBin (fromIntegral $ natVal (Proxy :: Proxy size))) (FixedByteString BS.empty)

instance ToSchema ByteString where
    toSchema = atom TyBin BS.empty
instance ToSchema Word16 where
    toSchema = atom TyWord16 0
instance ToSchema Word32 where
    toSchema = atom TyWord32 0
instance ToSchema Word64 where
    toSchema = atom TyWord64 0
instance ToSchema Bool where
    toSchema = atom TyBool False

instance ToSchema Connection
instance ToSchema MessageType
instance ToSchema ConferenceType
instance ToSchema FileControl
instance ToSchema UserStatus
instance ToSchema Event


main :: IO ()
main = do
--  putStrLn . groom . getSchema $ (toSchema :: Builder Event)
--  print . ppSchema . getSchema $ (toSchema :: Builder Event)
    print . genC . getSchema $ (toSchema :: Builder Event)
