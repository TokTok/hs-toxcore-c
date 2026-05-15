{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module FFI.Tox.Types (
    ConferenceType(..),
    Connection(..),
    ErrBootstrap(..),
    ErrConferenceById(..),
    ErrConferenceByUid(..),
    ErrConferenceDelete(..),
    ErrConferenceGetType(..),
    ErrConferenceInvite(..),
    ErrConferenceJoin(..),
    ErrConferenceNew(..),
    ErrConferencePeerQuery(..),
    ErrConferenceSendMessage(..),
    ErrConferenceSetMaxOffline(..),
    ErrConferenceTitle(..),
    ErrFileControl(..),
    ErrFileGet(..),
    ErrFileSeek(..),
    ErrFileSend(..),
    ErrFileSendChunk(..),
    ErrFriendAdd(..),
    ErrFriendByPublicKey(..),
    ErrFriendCustomPacket(..),
    ErrFriendDelete(..),
    ErrFriendGetLastOnline(..),
    ErrFriendGetPublicKey(..),
    ErrFriendQuery(..),
    ErrFriendSendMessage(..),
    ErrGetPort(..),
    ErrGroupDisconnect(..),
    ErrGroupInviteAccept(..),
    ErrGroupInviteFriend(..),
    ErrGroupIsConnected(..),
    ErrGroupJoin(..),
    ErrGroupKickPeer(..),
    ErrGroupLeave(..),
    ErrGroupNew(..),
    ErrGroupPeerQuery(..),
    ErrGroupReconnect(..),
    ErrGroupSelfNameSet(..),
    ErrGroupSelfQuery(..),
    ErrGroupSelfStatusSet(..),
    ErrGroupSendCustomPacket(..),
    ErrGroupSendCustomPrivatePacket(..),
    ErrGroupSendMessage(..),
    ErrGroupSendPrivateMessage(..),
    ErrGroupSetIgnore(..),
    ErrGroupSetPassword(..),
    ErrGroupSetPeerLimit(..),
    ErrGroupSetPrivacyState(..),
    ErrGroupSetRole(..),
    ErrGroupSetTopicLock(..),
    ErrGroupSetVoiceState(..),
    ErrGroupStateQuery(..),
    ErrGroupTopicSet(..),
    ErrNew(..),
    ErrOptionsNew(..),
    ErrSetInfo(..),
    ErrSetTyping(..),
    FileControl(..),
    FileKind(..),
    GroupExitType(..),
    GroupJoinFail(..),
    GroupModEvent(..),
    GroupPrivacyState(..),
    GroupRole(..),
    GroupTopicLock(..),
    GroupVoiceState(..),
    LogLevel(..),
    MessageType(..),
    ProxyType(..),
    SavedataType(..),
    UserStatus(..),
    ToxavCallControl(..),
    ToxavErrAnswer(..),
    ToxavErrBitRateSet(..),
    ToxavErrCall(..),
    ToxavErrCallControl(..),
    ToxavErrNew(..),
    ToxavErrSendFrame(..),
    ToxavFriendCallState(..),
    AvStruct,
    AvPtr,
    OptionsStruct,
    OptionsPtr,
    ToxStruct,
    ToxPtr,
    ConferenceNumber(..),
    ConferenceOfflinePeerNumber(..),
    ConferencePeerNumber(..),
    FileNumber(..),
    FriendMessageId(..),
    FriendNumber(..),
    GroupMessageId(..),
    GroupNumber(..),
    GroupPeerNumber(..),
    toxAddressSize_Const,
    toxConferenceIdSize_Const,
    toxConferenceUidSize_Const,
    toxFileIdLength_Const,
    toxGroupChatIdSize_Const,
    toxGroupMaxCustomLosslessPacketLength_Const,
    toxGroupMaxCustomLossyPacketLength_Const,
    toxGroupMaxGroupNameLength_Const,
    toxGroupMaxMessageLength_Const,
    toxGroupMaxPartLength_Const,
    toxGroupMaxPasswordSize_Const,
    toxGroupMaxTopicLength_Const,
    toxGroupPeerPublicKeySize_Const,
    toxHashLength_Const,
    toxMaxCustomPacketSize_Const,
    toxMaxFilenameLength_Const,
    toxMaxFriendRequestLength_Const,
    toxMaxHostnameLength_Const,
    toxMaxMessageLength_Const,
    toxMaxNameLength_Const,
    toxMaxStatusMessageLength_Const,
    toxNospamSize_Const,
    toxPublicKeySize_Const,
    toxSecretKeySize_Const,
    toxVersionMajor_Const,
    toxVersionMinor_Const,
    toxVersionPatch_Const,
    ConferenceConnectedCb,
    wrapConferenceConnectedCb,
    ConferenceInviteCb,
    wrapConferenceInviteCb,
    ConferenceMessageCb,
    wrapConferenceMessageCb,
    ConferencePeerListChangedCb,
    wrapConferencePeerListChangedCb,
    ConferencePeerNameCb,
    wrapConferencePeerNameCb,
    ConferenceTitleCb,
    wrapConferenceTitleCb,
    FileChunkRequestCb,
    wrapFileChunkRequestCb,
    FileRecvCb,
    wrapFileRecvCb,
    FileRecvChunkCb,
    wrapFileRecvChunkCb,
    FileRecvControlCb,
    wrapFileRecvControlCb,
    FriendConnectionStatusCb,
    wrapFriendConnectionStatusCb,
    FriendLosslessPacketCb,
    wrapFriendLosslessPacketCb,
    FriendLossyPacketCb,
    wrapFriendLossyPacketCb,
    FriendMessageCb,
    wrapFriendMessageCb,
    FriendNameCb,
    wrapFriendNameCb,
    FriendReadReceiptCb,
    wrapFriendReadReceiptCb,
    FriendRequestCb,
    wrapFriendRequestCb,
    FriendStatusCb,
    wrapFriendStatusCb,
    FriendStatusMessageCb,
    wrapFriendStatusMessageCb,
    FriendTypingCb,
    wrapFriendTypingCb,
    GroupCustomPacketCb,
    wrapGroupCustomPacketCb,
    GroupCustomPrivatePacketCb,
    wrapGroupCustomPrivatePacketCb,
    GroupInviteCb,
    wrapGroupInviteCb,
    GroupJoinFailCb,
    wrapGroupJoinFailCb,
    GroupMessageCb,
    wrapGroupMessageCb,
    GroupModerationCb,
    wrapGroupModerationCb,
    GroupPasswordCb,
    wrapGroupPasswordCb,
    GroupPeerExitCb,
    wrapGroupPeerExitCb,
    GroupPeerJoinCb,
    wrapGroupPeerJoinCb,
    GroupPeerLimitCb,
    wrapGroupPeerLimitCb,
    GroupPeerNameCb,
    wrapGroupPeerNameCb,
    GroupPeerStatusCb,
    wrapGroupPeerStatusCb,
    GroupPrivacyStateCb,
    wrapGroupPrivacyStateCb,
    GroupPrivateMessageCb,
    wrapGroupPrivateMessageCb,
    GroupSelfJoinCb,
    wrapGroupSelfJoinCb,
    GroupTopicCb,
    wrapGroupTopicCb,
    GroupTopicLockCb,
    wrapGroupTopicLockCb,
    GroupVoiceStateCb,
    wrapGroupVoiceStateCb,
    LogCb,
    wrapLogCb,
    SelfConnectionStatusCb,
    wrapSelfConnectionStatusCb,
    ToxavAudioBitRateCb,
    wrapToxavAudioBitRateCb,
    ToxavAudioDataCb,
    wrapToxavAudioDataCb,
    ToxavAudioReceiveFrameCb,
    wrapToxavAudioReceiveFrameCb,
    ToxavCallCb,
    wrapToxavCallCb,
    ToxavCallStateCb,
    wrapToxavCallStateCb,
    ToxavVideoBitRateCb,
    wrapToxavVideoBitRateCb,
    ToxavVideoReceiveFrameCb,
    wrapToxavVideoReceiveFrameCb
) where

import           Data.Int                  (Int16, Int32)
import           Data.MessagePack          (MessagePack)
import           Data.Word                 (Word16, Word32, Word64, Word8)
import           FFI.Tox.Raw
import           Foreign.C.Enum            (CEnum (..), CErr)
import           Foreign.C.String          (CString)
import           Foreign.C.Types           (CBool (..), CInt (..), CSize (..))
import           Foreign.Ptr               (FunPtr, Ptr)
import           Foreign.Storable          (Storable)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (..),
                                            arbitraryBoundedEnum)

instance MessagePack ConferenceType where

instance Arbitrary ConferenceType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack Connection where

instance Arbitrary Connection where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrBootstrap where

instance Arbitrary ErrBootstrap where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceById where

instance Arbitrary ErrConferenceById where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceByUid where

instance Arbitrary ErrConferenceByUid where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceDelete where

instance Arbitrary ErrConferenceDelete where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceGetType where

instance Arbitrary ErrConferenceGetType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceInvite where

instance Arbitrary ErrConferenceInvite where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceJoin where

instance Arbitrary ErrConferenceJoin where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceNew where

instance Arbitrary ErrConferenceNew where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferencePeerQuery where

instance Arbitrary ErrConferencePeerQuery where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceSendMessage where

instance Arbitrary ErrConferenceSendMessage where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceSetMaxOffline where

instance Arbitrary ErrConferenceSetMaxOffline where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrConferenceTitle where

instance Arbitrary ErrConferenceTitle where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFileControl where

instance Arbitrary ErrFileControl where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFileGet where

instance Arbitrary ErrFileGet where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFileSeek where

instance Arbitrary ErrFileSeek where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFileSend where

instance Arbitrary ErrFileSend where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFileSendChunk where

instance Arbitrary ErrFileSendChunk where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendAdd where

instance Arbitrary ErrFriendAdd where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendByPublicKey where

instance Arbitrary ErrFriendByPublicKey where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendCustomPacket where

instance Arbitrary ErrFriendCustomPacket where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendDelete where

instance Arbitrary ErrFriendDelete where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendGetLastOnline where

instance Arbitrary ErrFriendGetLastOnline where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendGetPublicKey where

instance Arbitrary ErrFriendGetPublicKey where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendQuery where

instance Arbitrary ErrFriendQuery where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrFriendSendMessage where

instance Arbitrary ErrFriendSendMessage where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGetPort where

instance Arbitrary ErrGetPort where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupDisconnect where

instance Arbitrary ErrGroupDisconnect where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupInviteAccept where

instance Arbitrary ErrGroupInviteAccept where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupInviteFriend where

instance Arbitrary ErrGroupInviteFriend where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupIsConnected where

instance Arbitrary ErrGroupIsConnected where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupJoin where

instance Arbitrary ErrGroupJoin where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupKickPeer where

instance Arbitrary ErrGroupKickPeer where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupLeave where

instance Arbitrary ErrGroupLeave where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupNew where

instance Arbitrary ErrGroupNew where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupPeerQuery where

instance Arbitrary ErrGroupPeerQuery where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupReconnect where

instance Arbitrary ErrGroupReconnect where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSelfNameSet where

instance Arbitrary ErrGroupSelfNameSet where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSelfQuery where

instance Arbitrary ErrGroupSelfQuery where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSelfStatusSet where

instance Arbitrary ErrGroupSelfStatusSet where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSendCustomPacket where

instance Arbitrary ErrGroupSendCustomPacket where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSendCustomPrivatePacket where

instance Arbitrary ErrGroupSendCustomPrivatePacket where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSendMessage where

instance Arbitrary ErrGroupSendMessage where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSendPrivateMessage where

instance Arbitrary ErrGroupSendPrivateMessage where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetIgnore where

instance Arbitrary ErrGroupSetIgnore where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetPassword where

instance Arbitrary ErrGroupSetPassword where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetPeerLimit where

instance Arbitrary ErrGroupSetPeerLimit where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetPrivacyState where

instance Arbitrary ErrGroupSetPrivacyState where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetRole where

instance Arbitrary ErrGroupSetRole where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetTopicLock where

instance Arbitrary ErrGroupSetTopicLock where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupSetVoiceState where

instance Arbitrary ErrGroupSetVoiceState where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupStateQuery where

instance Arbitrary ErrGroupStateQuery where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrGroupTopicSet where

instance Arbitrary ErrGroupTopicSet where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrNew where

instance Arbitrary ErrNew where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrOptionsNew where

instance Arbitrary ErrOptionsNew where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrSetInfo where

instance Arbitrary ErrSetInfo where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ErrSetTyping where

instance Arbitrary ErrSetTyping where
    arbitrary = arbitraryBoundedEnum

instance MessagePack FileControl where

instance Arbitrary FileControl where
    arbitrary = arbitraryBoundedEnum

instance MessagePack FileKind where

instance Arbitrary FileKind where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupExitType where

instance Arbitrary GroupExitType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupJoinFail where

instance Arbitrary GroupJoinFail where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupModEvent where

instance Arbitrary GroupModEvent where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupPrivacyState where

instance Arbitrary GroupPrivacyState where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupRole where

instance Arbitrary GroupRole where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupTopicLock where

instance Arbitrary GroupTopicLock where
    arbitrary = arbitraryBoundedEnum

instance MessagePack GroupVoiceState where

instance Arbitrary GroupVoiceState where
    arbitrary = arbitraryBoundedEnum

instance MessagePack LogLevel where

instance Arbitrary LogLevel where
    arbitrary = arbitraryBoundedEnum

instance MessagePack MessageType where

instance Arbitrary MessageType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ProxyType where

instance Arbitrary ProxyType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack SavedataType where

instance Arbitrary SavedataType where
    arbitrary = arbitraryBoundedEnum

instance MessagePack UserStatus where

instance Arbitrary UserStatus where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavCallControl where

instance Arbitrary ToxavCallControl where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrAnswer where

instance Arbitrary ToxavErrAnswer where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrBitRateSet where

instance Arbitrary ToxavErrBitRateSet where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrCall where

instance Arbitrary ToxavErrCall where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrCallControl where

instance Arbitrary ToxavErrCallControl where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrNew where

instance Arbitrary ToxavErrNew where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavErrSendFrame where

instance Arbitrary ToxavErrSendFrame where
    arbitrary = arbitraryBoundedEnum

instance MessagePack ToxavFriendCallState where

instance Arbitrary ToxavFriendCallState where
    arbitrary = arbitraryBoundedEnum

newtype ConferenceNumber = ConferenceNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack ConferenceNumber where

instance Arbitrary ConferenceNumber where
    arbitrary = ConferenceNumber <$> arbitrary

newtype ConferenceOfflinePeerNumber = ConferenceOfflinePeerNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack ConferenceOfflinePeerNumber where

instance Arbitrary ConferenceOfflinePeerNumber where
    arbitrary = ConferenceOfflinePeerNumber <$> arbitrary

newtype ConferencePeerNumber = ConferencePeerNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack ConferencePeerNumber where

instance Arbitrary ConferencePeerNumber where
    arbitrary = ConferencePeerNumber <$> arbitrary

newtype FileNumber = FileNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack FileNumber where

instance Arbitrary FileNumber where
    arbitrary = FileNumber <$> arbitrary

newtype FriendMessageId = FriendMessageId Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack FriendMessageId where

instance Arbitrary FriendMessageId where
    arbitrary = FriendMessageId <$> arbitrary

newtype FriendNumber = FriendNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack FriendNumber where

instance Arbitrary FriendNumber where
    arbitrary = FriendNumber <$> arbitrary

newtype GroupMessageId = GroupMessageId Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack GroupMessageId where

instance Arbitrary GroupMessageId where
    arbitrary = GroupMessageId <$> arbitrary

newtype GroupNumber = GroupNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack GroupNumber where

instance Arbitrary GroupNumber where
    arbitrary = GroupNumber <$> arbitrary

newtype GroupPeerNumber = GroupPeerNumber Word32
    deriving (Eq, Ord, Show, Read, Generic, Storable, Num, Enum)
instance MessagePack GroupPeerNumber where

instance Arbitrary GroupPeerNumber where
    arbitrary = GroupPeerNumber <$> arbitrary

toxAddressSize_Const :: Word32
toxAddressSize_Const  = 38
toxConferenceIdSize_Const :: Word32
toxConferenceIdSize_Const  = 32
toxConferenceUidSize_Const :: Word32
toxConferenceUidSize_Const  = 32
toxFileIdLength_Const :: Word32
toxFileIdLength_Const  = 32
toxGroupChatIdSize_Const :: Word32
toxGroupChatIdSize_Const  = 32
toxGroupMaxCustomLosslessPacketLength_Const :: Word32
toxGroupMaxCustomLosslessPacketLength_Const  = 1373
toxGroupMaxCustomLossyPacketLength_Const :: Word32
toxGroupMaxCustomLossyPacketLength_Const  = 1373
toxGroupMaxGroupNameLength_Const :: Word32
toxGroupMaxGroupNameLength_Const  = 48
toxGroupMaxMessageLength_Const :: Word32
toxGroupMaxMessageLength_Const  = 1372
toxGroupMaxPartLength_Const :: Word32
toxGroupMaxPartLength_Const  = 128
toxGroupMaxPasswordSize_Const :: Word32
toxGroupMaxPasswordSize_Const  = 32
toxGroupMaxTopicLength_Const :: Word32
toxGroupMaxTopicLength_Const  = 512
toxGroupPeerPublicKeySize_Const :: Word32
toxGroupPeerPublicKeySize_Const  = 32
toxHashLength_Const :: Word32
toxHashLength_Const  = 32
toxMaxCustomPacketSize_Const :: Word32
toxMaxCustomPacketSize_Const  = 1373
toxMaxFilenameLength_Const :: Word32
toxMaxFilenameLength_Const  = 255
toxMaxFriendRequestLength_Const :: Word32
toxMaxFriendRequestLength_Const  = 921
toxMaxHostnameLength_Const :: Word32
toxMaxHostnameLength_Const  = 255
toxMaxMessageLength_Const :: Word32
toxMaxMessageLength_Const  = 1372
toxMaxNameLength_Const :: Word32
toxMaxNameLength_Const  = 128
toxMaxStatusMessageLength_Const :: Word32
toxMaxStatusMessageLength_Const  = 1007
toxNospamSize_Const :: Word32
toxNospamSize_Const  = 4
toxPublicKeySize_Const :: Word32
toxPublicKeySize_Const  = 32
toxSecretKeySize_Const :: Word32
toxSecretKeySize_Const  = 32
toxVersionMajor_Const :: Word32
toxVersionMajor_Const  = 0
toxVersionMinor_Const :: Word32
toxVersionMinor_Const  = 2
toxVersionPatch_Const :: Word32
toxVersionPatch_Const  = 21
type ConferenceConnectedCb = ToxPtr -> ConferenceNumber -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceConnectedCb :: ConferenceConnectedCb -> IO (FunPtr ConferenceConnectedCb)
type ConferenceInviteCb = ToxPtr -> FriendNumber -> (CEnum ConferenceType) -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceInviteCb :: ConferenceInviteCb -> IO (FunPtr ConferenceInviteCb)
type ConferenceMessageCb = ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> (CEnum MessageType) -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceMessageCb :: ConferenceMessageCb -> IO (FunPtr ConferenceMessageCb)
type ConferencePeerListChangedCb = ToxPtr -> ConferenceNumber -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferencePeerListChangedCb :: ConferencePeerListChangedCb -> IO (FunPtr ConferencePeerListChangedCb)
type ConferencePeerNameCb = ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferencePeerNameCb :: ConferencePeerNameCb -> IO (FunPtr ConferencePeerNameCb)
type ConferenceTitleCb = ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceTitleCb :: ConferenceTitleCb -> IO (FunPtr ConferenceTitleCb)
type FileChunkRequestCb = ToxPtr -> FriendNumber -> FileNumber -> Word64 -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileChunkRequestCb :: FileChunkRequestCb -> IO (FunPtr FileChunkRequestCb)
type FileRecvCb = ToxPtr -> FriendNumber -> FileNumber -> Word32 -> Word64 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvCb :: FileRecvCb -> IO (FunPtr FileRecvCb)
type FileRecvChunkCb = ToxPtr -> FriendNumber -> FileNumber -> Word64 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvChunkCb :: FileRecvChunkCb -> IO (FunPtr FileRecvChunkCb)
type FileRecvControlCb = ToxPtr -> FriendNumber -> FileNumber -> (CEnum FileControl) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvControlCb :: FileRecvControlCb -> IO (FunPtr FileRecvControlCb)
type FriendConnectionStatusCb = ToxPtr -> FriendNumber -> (CEnum Connection) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendConnectionStatusCb :: FriendConnectionStatusCb -> IO (FunPtr FriendConnectionStatusCb)
type FriendLosslessPacketCb = ToxPtr -> FriendNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendLosslessPacketCb :: FriendLosslessPacketCb -> IO (FunPtr FriendLosslessPacketCb)
type FriendLossyPacketCb = ToxPtr -> FriendNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendLossyPacketCb :: FriendLossyPacketCb -> IO (FunPtr FriendLossyPacketCb)
type FriendMessageCb = ToxPtr -> FriendNumber -> (CEnum MessageType) -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendMessageCb :: FriendMessageCb -> IO (FunPtr FriendMessageCb)
type FriendNameCb = ToxPtr -> FriendNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendNameCb :: FriendNameCb -> IO (FunPtr FriendNameCb)
type FriendReadReceiptCb = ToxPtr -> FriendNumber -> FriendMessageId -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendReadReceiptCb :: FriendReadReceiptCb -> IO (FunPtr FriendReadReceiptCb)
type FriendRequestCb = ToxPtr -> CString -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendRequestCb :: FriendRequestCb -> IO (FunPtr FriendRequestCb)
type FriendStatusCb = ToxPtr -> FriendNumber -> (CEnum UserStatus) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendStatusCb :: FriendStatusCb -> IO (FunPtr FriendStatusCb)
type FriendStatusMessageCb = ToxPtr -> FriendNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendStatusMessageCb :: FriendStatusMessageCb -> IO (FunPtr FriendStatusMessageCb)
type FriendTypingCb = ToxPtr -> FriendNumber -> CBool -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendTypingCb :: FriendTypingCb -> IO (FunPtr FriendTypingCb)
type GroupCustomPacketCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupCustomPacketCb :: GroupCustomPacketCb -> IO (FunPtr GroupCustomPacketCb)
type GroupCustomPrivatePacketCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupCustomPrivatePacketCb :: GroupCustomPrivatePacketCb -> IO (FunPtr GroupCustomPrivatePacketCb)
type GroupInviteCb = ToxPtr -> FriendNumber -> CString -> CSize -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupInviteCb :: GroupInviteCb -> IO (FunPtr GroupInviteCb)
type GroupJoinFailCb = ToxPtr -> GroupNumber -> (CEnum GroupJoinFail) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupJoinFailCb :: GroupJoinFailCb -> IO (FunPtr GroupJoinFailCb)
type GroupMessageCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> (CEnum MessageType) -> CString -> CSize -> GroupMessageId -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupMessageCb :: GroupMessageCb -> IO (FunPtr GroupMessageCb)
type GroupModerationCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> GroupPeerNumber -> (CEnum GroupModEvent) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupModerationCb :: GroupModerationCb -> IO (FunPtr GroupModerationCb)
type GroupPasswordCb = ToxPtr -> GroupNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPasswordCb :: GroupPasswordCb -> IO (FunPtr GroupPasswordCb)
type GroupPeerExitCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> (CEnum GroupExitType) -> CString -> CSize -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerExitCb :: GroupPeerExitCb -> IO (FunPtr GroupPeerExitCb)
type GroupPeerJoinCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerJoinCb :: GroupPeerJoinCb -> IO (FunPtr GroupPeerJoinCb)
type GroupPeerLimitCb = ToxPtr -> GroupNumber -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerLimitCb :: GroupPeerLimitCb -> IO (FunPtr GroupPeerLimitCb)
type GroupPeerNameCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerNameCb :: GroupPeerNameCb -> IO (FunPtr GroupPeerNameCb)
type GroupPeerStatusCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> (CEnum UserStatus) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerStatusCb :: GroupPeerStatusCb -> IO (FunPtr GroupPeerStatusCb)
type GroupPrivacyStateCb = ToxPtr -> GroupNumber -> (CEnum GroupPrivacyState) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPrivacyStateCb :: GroupPrivacyStateCb -> IO (FunPtr GroupPrivacyStateCb)
type GroupPrivateMessageCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> (CEnum MessageType) -> CString -> CSize -> GroupMessageId -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPrivateMessageCb :: GroupPrivateMessageCb -> IO (FunPtr GroupPrivateMessageCb)
type GroupSelfJoinCb = ToxPtr -> GroupNumber -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupSelfJoinCb :: GroupSelfJoinCb -> IO (FunPtr GroupSelfJoinCb)
type GroupTopicCb = ToxPtr -> GroupNumber -> GroupPeerNumber -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupTopicCb :: GroupTopicCb -> IO (FunPtr GroupTopicCb)
type GroupTopicLockCb = ToxPtr -> GroupNumber -> (CEnum GroupTopicLock) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupTopicLockCb :: GroupTopicLockCb -> IO (FunPtr GroupTopicLockCb)
type GroupVoiceStateCb = ToxPtr -> GroupNumber -> (CEnum GroupVoiceState) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupVoiceStateCb :: GroupVoiceStateCb -> IO (FunPtr GroupVoiceStateCb)
type LogCb = ToxPtr -> (CEnum LogLevel) -> CString -> Word32 -> CString -> CString -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapLogCb :: LogCb -> IO (FunPtr LogCb)
type SelfConnectionStatusCb = ToxPtr -> (CEnum Connection) -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapSelfConnectionStatusCb :: SelfConnectionStatusCb -> IO (FunPtr SelfConnectionStatusCb)
type ToxavAudioBitRateCb = AvPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavAudioBitRateCb :: ToxavAudioBitRateCb -> IO (FunPtr ToxavAudioBitRateCb)
type ToxavAudioDataCb = Ptr () -> ConferenceNumber -> ConferencePeerNumber -> Ptr Int16 -> Word32 -> Word8 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavAudioDataCb :: ToxavAudioDataCb -> IO (FunPtr ToxavAudioDataCb)
type ToxavAudioReceiveFrameCb = AvPtr -> Word32 -> Ptr Int16 -> CSize -> Word8 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavAudioReceiveFrameCb :: ToxavAudioReceiveFrameCb -> IO (FunPtr ToxavAudioReceiveFrameCb)
type ToxavCallCb = AvPtr -> Word32 -> CBool -> CBool -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavCallCb :: ToxavCallCb -> IO (FunPtr ToxavCallCb)
type ToxavCallStateCb = AvPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavCallStateCb :: ToxavCallStateCb -> IO (FunPtr ToxavCallStateCb)
type ToxavVideoBitRateCb = AvPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavVideoBitRateCb :: ToxavVideoBitRateCb -> IO (FunPtr ToxavVideoBitRateCb)
type ToxavVideoReceiveFrameCb = AvPtr -> Word32 -> Word16 -> Word16 -> CString -> CString -> CString -> Int32 -> Int32 -> Int32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapToxavVideoReceiveFrameCb :: ToxavVideoReceiveFrameCb -> IO (FunPtr ToxavVideoReceiveFrameCb)
