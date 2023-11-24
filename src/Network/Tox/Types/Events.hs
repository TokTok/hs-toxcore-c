{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Network.Tox.Types.Events where

import qualified Data.ByteString                      as BS
import           Data.MessagePack                     (MessagePack)
import           Data.Word                            (Word16, Word32, Word64)
import           FFI.Tox.Tox                          (ConferenceType,
                                                       Connection, FileControl,
                                                       MessageType, UserStatus)
import           GHC.Generics                         (Generic)
import           Network.Tox.C.Type                   (PublicKey)
import           Test.QuickCheck.Arbitrary            (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic    (genericArbitrary,
                                                       genericShrink)
import           Test.QuickCheck.Instances.ByteString ()


data Event
    = SelfConnectionStatus      { connectionStatus :: Connection }

    | FriendRequest             { publicKey :: PublicKey, message :: BS.ByteString }
    | FriendConnectionStatus    { friendNumber :: Word32, connectionStatus :: Connection }
    | FriendLossyPacket         { friendNumber :: Word32, data' :: BS.ByteString }
    | FriendLosslessPacket      { friendNumber :: Word32, data' :: BS.ByteString }

    | FriendName                { friendNumber :: Word32, name :: BS.ByteString }
    | FriendStatus              { friendNumber :: Word32, status :: UserStatus }
    | FriendStatusMessage       { friendNumber :: Word32, statusMessage :: BS.ByteString }

    | FriendMessage             { friendNumber :: Word32, messageType :: MessageType, message :: BS.ByteString }
    | FriendReadReceipt         { friendNumber :: Word32, messageId :: Word32 }
    | FriendTyping              { friendNumber :: Word32, typing :: Bool }

    | FileChunkRequest          { friendNumber :: Word32, fileNumber :: Word32, position :: Word64, length :: Word16 }
    | FileRecv                  { friendNumber :: Word32, fileNumber :: Word32, kind :: Word32, fileSize :: Word64, filename :: BS.ByteString }
    | FileRecvChunk             { friendNumber :: Word32, fileNumber :: Word32, position :: Word64, data' ::  BS.ByteString }
    | FileRecvControl           { friendNumber :: Word32, fileNumber :: Word32, control :: FileControl }

    | ConferenceInvite          { friendNumber :: Word32, conferenceType :: ConferenceType, cookie :: BS.ByteString }
    | ConferenceConnected       { conferenceNumber :: Word32 }
    | ConferencePeerListChanged { conferenceNumber :: Word32 }
    | ConferencePeerName        { conferenceNumber :: Word32, peerNumber :: Word32, name :: BS.ByteString }
    | ConferenceTitle           { conferenceNumber :: Word32, peerNumber :: Word32, title :: BS.ByteString }

    | ConferenceMessage         { conferenceNumber :: Word32, peerNumber :: Word32, messageType :: MessageType, message :: BS.ByteString }
    deriving (Ord, Eq, Show, Generic)

instance MessagePack Event
instance Arbitrary Event where
    arbitrary = genericArbitrary
    shrink = genericShrink

data Events = Events [Event]
    deriving (Ord, Eq, Show, Generic)

instance MessagePack Events
instance Arbitrary Events where
    arbitrary = genericArbitrary
    shrink = genericShrink
