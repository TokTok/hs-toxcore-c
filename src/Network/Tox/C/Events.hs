{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Network.Tox.C.Events where

import qualified Data.ByteString                      as BS
import           Data.Word                            (Word16, Word32, Word64)
import           GHC.Generics                         (Generic)
import           Test.QuickCheck.Arbitrary            (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic    (genericArbitrary,
                                                       genericShrink)
import           Test.QuickCheck.Instances.ByteString ()

import           Data.MessagePack


newtype PublicKey = PublicKey BS.ByteString
    deriving (Ord, Eq, Show, Generic)

instance MessagePack PublicKey

instance Arbitrary PublicKey where
    arbitrary = pure $ PublicKey "00000000000000000000000000000000"

data Event
    = SelfConnectionStatus      Word32

    | FriendRequest             PublicKey BS.ByteString
    | FriendConnectionStatus    Word32 Word32
    | FriendLossyPacket         Word32 BS.ByteString
    | FriendLosslessPacket      Word32 BS.ByteString

    | FriendName                Word32 BS.ByteString
    | FriendStatus              Word32 Word32
    | FriendStatusMessage       Word32 BS.ByteString

    | FriendMessage             Word32 Word32 BS.ByteString
    | FriendReadReceipt         Word32 Word32
    | FriendTyping              Word32 Bool

    | FileChunkRequest          Word32 Word32 Word64 Word16
    | FileRecv                  Word32 Word32 Word32 Word64 BS.ByteString
    | FileRecvChunk             Word32 Word32 Word64 BS.ByteString
    | FileRecvControl           Word32 Word32 Word32

    | ConferenceInvite          Word32 Word32 BS.ByteString
    | ConferenceConnected       Word32
    | ConferencePeerListChanged Word32
    | ConferencePeerName        Word32 Word32 BS.ByteString
    | ConferenceTitle           Word32 Word32 BS.ByteString

    | ConferenceMessage Word32 Word32 Word32 BS.ByteString
    deriving (Ord, Eq, Show, Generic)

instance MessagePack Event

instance Arbitrary Event where
    arbitrary = genericArbitrary
    shrink = genericShrink
