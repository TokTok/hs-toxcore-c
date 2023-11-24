{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Network.Tox.C.Type where

import qualified Data.ByteString           as BS
import           Data.MessagePack          (MessagePack)
import           GHC.Generics              (Generic)
import           GHC.TypeNats              (Nat)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))


newtype FixedByteString (size :: Nat) = FixedByteString BS.ByteString
    deriving (Ord, Eq, Show, Generic)

instance MessagePack (FixedByteString size)

instance Arbitrary (FixedByteString size) where
    arbitrary = pure $ FixedByteString "00000000000000000000000000000000"


type PublicKey = FixedByteString 32
