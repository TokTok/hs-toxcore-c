{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE StrictData        #-}
module Network.Tox.C.Type where

import qualified Data.ByteString           as BS
import           Data.MessagePack          (MessagePack)
import           GHC.Generics              (Generic)
import           GHC.TypeNats              (Nat)
import           Test.QuickCheck.Arbitrary (Arbitrary (..),
                                            arbitraryBoundedEnum)


newtype FixedByteString (size :: Nat) = FixedByteString BS.ByteString
    deriving (Ord, Eq, Show, Generic)

instance MessagePack (FixedByteString size)

instance Arbitrary (FixedByteString size) where
    arbitrary = pure $ FixedByteString "00000000000000000000000000000000"


type PublicKey = FixedByteString 32


-- | Protocols that can be used to connect to the network or friends.
data Connection
  = ConnectionNone
    -- There is no connection. This instance, or the friend the state change is
    -- about, is now offline.

  | ConnectionTcp
    -- A TCP connection has been established. For the own instance, this means
    -- it is connected through a TCP relay, only. For a friend, this means that
    -- the connection to that particular friend goes through a TCP relay.

  | ConnectionUdp
    -- A UDP connection has been established. For the own instance, this means
    -- it is able to send UDP packets to DHT nodes, but may still be connected
    -- to a TCP relay. For a friend, this means that the connection to that
    -- particular friend was built using direct UDP packets.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance MessagePack Connection
instance Arbitrary Connection where
    arbitrary = arbitraryBoundedEnum


-- | Represents the possible statuses a client can have.
data UserStatus
  = UserStatusNone
    -- ^ User is online and available.
  | UserStatusAway
    -- ^ User is away. Clients can set this e.g. after a user defined inactivity
    -- time.
  | UserStatusBusy
    -- ^ User is busy. Signals to other clients that this client does not
    -- currently wish to communicate.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance MessagePack UserStatus
instance Arbitrary UserStatus where
    arbitrary = arbitraryBoundedEnum


-- | Represents message types for tox_friend_send_message and group chat
-- messages.
data MessageType
  = MessageTypeNormal
    -- ^ Normal text message. Similar to PRIVMSG on IRC.
  | MessageTypeAction
    -- ^ A message describing an user action. This is similar to /me (CTCP
    -- ACTION) on IRC.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance MessagePack MessageType
instance Arbitrary MessageType where
    arbitrary = arbitraryBoundedEnum


data FileControl
  = FileControlResume
    -- Sent by the receiving side to accept a file send request. Also sent after
    -- a FileControlPause command to continue sending or receiving.

  | FileControlPause
    -- Sent by clients to pause the file transfer. The initial state of a file
    -- transfer is always paused on the receiving side and running on the
    -- sending side. If both the sending and receiving side pause the transfer,
    -- then both need to send FileControlResume for the transfer to resume.

  | FileControlCancel
    -- Sent by the receiving side to reject a file send request before any other
    -- commands are sent. Also sent by either side to terminate a file transfer.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance MessagePack FileControl
instance Arbitrary FileControl where
    arbitrary = arbitraryBoundedEnum


-- | Conference types for the conference_invite event.
data ConferenceType
  = ConferenceTypeText
    -- Text-only conferences that must be accepted with the tox_conference_join function.

  | ConferenceTypeAv
    -- Video conference. The function to accept these is in toxav.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance MessagePack ConferenceType
instance Arbitrary ConferenceType where
    arbitrary = arbitraryBoundedEnum
