{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.Core where

import           Control.Monad         ((>=>))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Int              (Int32)
import           Data.MessagePack      (MessagePack)
import           Data.Word             (Word16, Word32, Word64, Word8)
import qualified FFI.Tox.Raw           as Raw
import           FFI.Tox.Types
import           Foreign.C.Enum        (CEnum (..), CErr, callErrFun, fromCEnum,
                                        toCEnum)
import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types       (CBool (..), CInt (..), CSize (..))
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (fromBool, toBool)
import           Foreign.Ptr           (FunPtr, Ptr, castFunPtrToPtr, castPtr,
                                        castPtrToFunPtr, nullPtr)
import           Foreign.Storable      (Storable)
import           GHC.Generics          (Generic)

toxavGetTox :: AvPtr -> IO ToxPtr
toxavGetTox av = Raw.toxavGetTox av
toxVersionMajor :: IO Word32
toxVersionMajor  = fmap fromIntegral Raw.toxVersionMajor
toxVersionMinor :: IO Word32
toxVersionMinor  = fmap fromIntegral Raw.toxVersionMinor
toxVersionPatch :: IO Word32
toxVersionPatch  = fmap fromIntegral Raw.toxVersionPatch
toxVersionIsCompatible :: Word32 -> Word32 -> Word32 -> IO Bool
toxVersionIsCompatible major minor patch = Raw.toxVersionIsCompatible (fromIntegral major) (fromIntegral minor) (fromIntegral patch)
toxPublicKeySize :: IO Word32
toxPublicKeySize  = fmap fromIntegral Raw.toxPublicKeySize
toxSecretKeySize :: IO Word32
toxSecretKeySize  = fmap fromIntegral Raw.toxSecretKeySize
toxNospamSize :: IO Word32
toxNospamSize  = fmap fromIntegral Raw.toxNospamSize
toxAddressSize :: IO Word32
toxAddressSize  = fmap fromIntegral Raw.toxAddressSize
toxMaxNameLength :: IO Word32
toxMaxNameLength  = fmap fromIntegral Raw.toxMaxNameLength
toxMaxStatusMessageLength :: IO Word32
toxMaxStatusMessageLength  = fmap fromIntegral Raw.toxMaxStatusMessageLength
toxMaxFriendRequestLength :: IO Word32
toxMaxFriendRequestLength  = fmap fromIntegral Raw.toxMaxFriendRequestLength
toxMaxMessageLength :: IO Word32
toxMaxMessageLength  = fmap fromIntegral Raw.toxMaxMessageLength
toxMaxCustomPacketSize :: IO Word32
toxMaxCustomPacketSize  = fmap fromIntegral Raw.toxMaxCustomPacketSize
toxHashLength :: IO Word32
toxHashLength  = fmap fromIntegral Raw.toxHashLength
toxMaxFilenameLength :: IO Word32
toxMaxFilenameLength  = fmap fromIntegral Raw.toxMaxFilenameLength
toxMaxHostnameLength :: IO Word32
toxMaxHostnameLength  = fmap fromIntegral Raw.toxMaxHostnameLength
toxUserStatusToString :: UserStatus -> IO ByteString
toxUserStatusToString value = Raw.toxUserStatusToString value >>= BS.packCString
toxMessageTypeToString :: MessageType -> IO ByteString
toxMessageTypeToString value = Raw.toxMessageTypeToString value >>= BS.packCString
toxErrNewToString :: ErrNew -> IO ByteString
toxErrNewToString value = Raw.toxErrNewToString value >>= BS.packCString
toxNew :: OptionsPtr -> IO (Either ErrNew ToxPtr)
toxNew options = callErrFun (\errPtr -> Raw.toxNew options (castPtr errPtr))
toxKill :: ToxPtr -> IO ()
toxKill this = Raw.toxKill this
toxGetSavedataSize :: ToxPtr -> IO CSize
toxGetSavedataSize this = fmap fromIntegral (Raw.toxGetSavedataSize this)
toxGetSavedata :: ToxPtr -> IO ByteString
toxGetSavedata this = do { size <- Raw.toxGetSavedataSize this; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxGetSavedata this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxErrBootstrapToString :: ErrBootstrap -> IO ByteString
toxErrBootstrapToString value = Raw.toxErrBootstrapToString value >>= BS.packCString
toxBootstrap :: ToxPtr -> ByteString -> Word16 -> ByteString -> IO (Either ErrBootstrap Bool)
toxBootstrap this host port publicKey = BS.useAsCString host (\ptr0 -> BS.useAsCString publicKey (\ptr2 -> callErrFun (\errPtr -> Raw.toxBootstrap this ptr0 (fromIntegral port) (castPtr ptr2) (castPtr errPtr))))
toxAddTcpRelay :: ToxPtr -> ByteString -> Word16 -> ByteString -> IO (Either ErrBootstrap Bool)
toxAddTcpRelay this host port publicKey = BS.useAsCString host (\ptr0 -> BS.useAsCString publicKey (\ptr2 -> callErrFun (\errPtr -> Raw.toxAddTcpRelay this ptr0 (fromIntegral port) (castPtr ptr2) (castPtr errPtr))))
toxConnectionToString :: Connection -> IO ByteString
toxConnectionToString value = Raw.toxConnectionToString value >>= BS.packCString
toxSelfGetConnectionStatus :: ToxPtr -> IO Connection
toxSelfGetConnectionStatus this = Raw.toxSelfGetConnectionStatus this
toxIterationInterval :: ToxPtr -> IO Word32
toxIterationInterval this = fmap fromIntegral (Raw.toxIterationInterval this)
toxIterate :: ToxPtr -> IO ()
toxIterate this = Raw.toxIterate this nullPtr
toxSelfGetAddress :: ToxPtr -> IO ByteString
toxSelfGetAddress this = do { let { size  = FFI.Tox.Types.toxAddressSize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetAddress this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxSelfSetNospam :: ToxPtr -> Word32 -> IO ()
toxSelfSetNospam this nospam = Raw.toxSelfSetNospam this (fromIntegral nospam)
toxSelfGetNospam :: ToxPtr -> IO Word32
toxSelfGetNospam this = fmap fromIntegral (Raw.toxSelfGetNospam this)
toxSelfGetPublicKey :: ToxPtr -> IO ByteString
toxSelfGetPublicKey this = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetPublicKey this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxSelfGetSecretKey :: ToxPtr -> IO ByteString
toxSelfGetSecretKey this = do { let { size  = FFI.Tox.Types.toxSecretKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetSecretKey this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxErrSetInfoToString :: ErrSetInfo -> IO ByteString
toxErrSetInfoToString value = Raw.toxErrSetInfoToString value >>= BS.packCString
toxSelfSetName :: ToxPtr -> ByteString -> IO (Either ErrSetInfo Bool)
toxSelfSetName this name = BS.useAsCStringLen name (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxSelfSetName this (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxSelfGetNameSize :: ToxPtr -> IO CSize
toxSelfGetNameSize this = fmap fromIntegral (Raw.toxSelfGetNameSize this)
toxSelfGetName :: ToxPtr -> IO ByteString
toxSelfGetName this = do { size <- Raw.toxSelfGetNameSize this; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetName this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxSelfSetStatusMessage :: ToxPtr -> ByteString -> IO (Either ErrSetInfo Bool)
toxSelfSetStatusMessage this statusMessage = BS.useAsCStringLen statusMessage (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxSelfSetStatusMessage this (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxSelfGetStatusMessageSize :: ToxPtr -> IO CSize
toxSelfGetStatusMessageSize this = fmap fromIntegral (Raw.toxSelfGetStatusMessageSize this)
toxSelfGetStatusMessage :: ToxPtr -> IO ByteString
toxSelfGetStatusMessage this = do { size <- Raw.toxSelfGetStatusMessageSize this; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetStatusMessage this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxSelfSetStatus :: ToxPtr -> UserStatus -> IO ()
toxSelfSetStatus this status = Raw.toxSelfSetStatus this status
toxSelfGetStatus :: ToxPtr -> IO UserStatus
toxSelfGetStatus this = Raw.toxSelfGetStatus this
toxErrFriendAddToString :: ErrFriendAdd -> IO ByteString
toxErrFriendAddToString value = Raw.toxErrFriendAddToString value >>= BS.packCString
toxErrFriendDeleteToString :: ErrFriendDelete -> IO ByteString
toxErrFriendDeleteToString value = Raw.toxErrFriendDeleteToString value >>= BS.packCString
toxErrFriendByPublicKeyToString :: ErrFriendByPublicKey -> IO ByteString
toxErrFriendByPublicKeyToString value = Raw.toxErrFriendByPublicKeyToString value >>= BS.packCString
toxSelfGetFriendListSize :: ToxPtr -> IO CSize
toxSelfGetFriendListSize this = fmap fromIntegral (Raw.toxSelfGetFriendListSize this)
toxSelfGetFriendList :: ToxPtr -> IO [FriendNumber]
toxSelfGetFriendList this = do { size <- Raw.toxSelfGetFriendListSize this; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetFriendList this outPtr; val <- peekArray (fromIntegral size) (castPtr outPtr); return val }) }
toxErrFriendGetPublicKeyToString :: ErrFriendGetPublicKey -> IO ByteString
toxErrFriendGetPublicKeyToString value = Raw.toxErrFriendGetPublicKeyToString value >>= BS.packCString
toxErrFriendGetLastOnlineToString :: ErrFriendGetLastOnline -> IO ByteString
toxErrFriendGetLastOnlineToString value = Raw.toxErrFriendGetLastOnlineToString value >>= BS.packCString
toxErrFriendQueryToString :: ErrFriendQuery -> IO ByteString
toxErrFriendQueryToString value = Raw.toxErrFriendQueryToString value >>= BS.packCString
toxErrSetTypingToString :: ErrSetTyping -> IO ByteString
toxErrSetTypingToString value = Raw.toxErrSetTypingToString value >>= BS.packCString
toxSelfSetTyping :: ToxPtr -> FriendNumber -> Bool -> IO (Either ErrSetTyping Bool)
toxSelfSetTyping this (FriendNumber friendNumber) typing = callErrFun (\errPtr -> Raw.toxSelfSetTyping this friendNumber typing (castPtr errPtr))
toxErrFriendSendMessageToString :: ErrFriendSendMessage -> IO ByteString
toxErrFriendSendMessageToString value = Raw.toxErrFriendSendMessageToString value >>= BS.packCString
toxHash :: ByteString -> IO ByteString
toxHash data_ = BS.useAsCStringLen data_ (\(ptr0, len0) -> do { let { size  = FFI.Tox.Types.toxHashLength_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxHash outPtr (castPtr ptr0) (fromIntegral len0); val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) })
toxErrFileControlToString :: ErrFileControl -> IO ByteString
toxErrFileControlToString value = Raw.toxErrFileControlToString value >>= BS.packCString
toxErrFileSeekToString :: ErrFileSeek -> IO ByteString
toxErrFileSeekToString value = Raw.toxErrFileSeekToString value >>= BS.packCString
toxErrFileGetToString :: ErrFileGet -> IO ByteString
toxErrFileGetToString value = Raw.toxErrFileGetToString value >>= BS.packCString
toxErrFileSendToString :: ErrFileSend -> IO ByteString
toxErrFileSendToString value = Raw.toxErrFileSendToString value >>= BS.packCString
toxErrFileSendChunkToString :: ErrFileSendChunk -> IO ByteString
toxErrFileSendChunkToString value = Raw.toxErrFileSendChunkToString value >>= BS.packCString
toxErrConferenceNewToString :: ErrConferenceNew -> IO ByteString
toxErrConferenceNewToString value = Raw.toxErrConferenceNewToString value >>= BS.packCString
toxErrConferenceDeleteToString :: ErrConferenceDelete -> IO ByteString
toxErrConferenceDeleteToString value = Raw.toxErrConferenceDeleteToString value >>= BS.packCString
toxErrConferencePeerQueryToString :: ErrConferencePeerQuery -> IO ByteString
toxErrConferencePeerQueryToString value = Raw.toxErrConferencePeerQueryToString value >>= BS.packCString
toxErrConferenceSetMaxOfflineToString :: ErrConferenceSetMaxOffline -> IO ByteString
toxErrConferenceSetMaxOfflineToString value = Raw.toxErrConferenceSetMaxOfflineToString value >>= BS.packCString
toxErrConferenceInviteToString :: ErrConferenceInvite -> IO ByteString
toxErrConferenceInviteToString value = Raw.toxErrConferenceInviteToString value >>= BS.packCString
toxConferenceInvite :: ToxPtr -> FriendNumber -> ConferenceNumber -> IO (Either ErrConferenceInvite Bool)
toxConferenceInvite this (FriendNumber friendNumber) (ConferenceNumber conferenceNumber) = callErrFun (\errPtr -> Raw.toxConferenceInvite this friendNumber conferenceNumber (castPtr errPtr))
toxErrConferenceJoinToString :: ErrConferenceJoin -> IO ByteString
toxErrConferenceJoinToString value = Raw.toxErrConferenceJoinToString value >>= BS.packCString
toxErrConferenceSendMessageToString :: ErrConferenceSendMessage -> IO ByteString
toxErrConferenceSendMessageToString value = Raw.toxErrConferenceSendMessageToString value >>= BS.packCString
toxErrConferenceTitleToString :: ErrConferenceTitle -> IO ByteString
toxErrConferenceTitleToString value = Raw.toxErrConferenceTitleToString value >>= BS.packCString
toxErrConferenceGetTypeToString :: ErrConferenceGetType -> IO ByteString
toxErrConferenceGetTypeToString value = Raw.toxErrConferenceGetTypeToString value >>= BS.packCString
toxErrConferenceByIdToString :: ErrConferenceById -> IO ByteString
toxErrConferenceByIdToString value = Raw.toxErrConferenceByIdToString value >>= BS.packCString
toxErrConferenceByUidToString :: ErrConferenceByUid -> IO ByteString
toxErrConferenceByUidToString value = Raw.toxErrConferenceByUidToString value >>= BS.packCString
toxErrFriendCustomPacketToString :: ErrFriendCustomPacket -> IO ByteString
toxErrFriendCustomPacketToString value = Raw.toxErrFriendCustomPacketToString value >>= BS.packCString
toxErrGetPortToString :: ErrGetPort -> IO ByteString
toxErrGetPortToString value = Raw.toxErrGetPortToString value >>= BS.packCString
toxSelfGetDhtId :: ToxPtr -> IO ByteString
toxSelfGetDhtId this = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxSelfGetDhtId this outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxSelfGetUdpPort :: ToxPtr -> IO (Either ErrGetPort Word16)
toxSelfGetUdpPort this = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxSelfGetUdpPort this (castPtr errPtr)))
toxSelfGetTcpPort :: ToxPtr -> IO (Either ErrGetPort Word16)
toxSelfGetTcpPort this = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxSelfGetTcpPort this (castPtr errPtr)))
toxErrGroupNewToString :: ErrGroupNew -> IO ByteString
toxErrGroupNewToString value = Raw.toxErrGroupNewToString value >>= BS.packCString
toxErrGroupJoinToString :: ErrGroupJoin -> IO ByteString
toxErrGroupJoinToString value = Raw.toxErrGroupJoinToString value >>= BS.packCString
toxErrGroupIsConnectedToString :: ErrGroupIsConnected -> IO ByteString
toxErrGroupIsConnectedToString value = Raw.toxErrGroupIsConnectedToString value >>= BS.packCString
toxErrGroupDisconnectToString :: ErrGroupDisconnect -> IO ByteString
toxErrGroupDisconnectToString value = Raw.toxErrGroupDisconnectToString value >>= BS.packCString
toxErrGroupReconnectToString :: ErrGroupReconnect -> IO ByteString
toxErrGroupReconnectToString value = Raw.toxErrGroupReconnectToString value >>= BS.packCString
toxErrGroupLeaveToString :: ErrGroupLeave -> IO ByteString
toxErrGroupLeaveToString value = Raw.toxErrGroupLeaveToString value >>= BS.packCString
toxErrGroupSelfQueryToString :: ErrGroupSelfQuery -> IO ByteString
toxErrGroupSelfQueryToString value = Raw.toxErrGroupSelfQueryToString value >>= BS.packCString
toxErrGroupSelfNameSetToString :: ErrGroupSelfNameSet -> IO ByteString
toxErrGroupSelfNameSetToString value = Raw.toxErrGroupSelfNameSetToString value >>= BS.packCString
toxErrGroupSelfStatusSetToString :: ErrGroupSelfStatusSet -> IO ByteString
toxErrGroupSelfStatusSetToString value = Raw.toxErrGroupSelfStatusSetToString value >>= BS.packCString
toxErrGroupPeerQueryToString :: ErrGroupPeerQuery -> IO ByteString
toxErrGroupPeerQueryToString value = Raw.toxErrGroupPeerQueryToString value >>= BS.packCString
toxErrGroupStateQueryToString :: ErrGroupStateQuery -> IO ByteString
toxErrGroupStateQueryToString value = Raw.toxErrGroupStateQueryToString value >>= BS.packCString
toxErrGroupTopicSetToString :: ErrGroupTopicSet -> IO ByteString
toxErrGroupTopicSetToString value = Raw.toxErrGroupTopicSetToString value >>= BS.packCString
toxErrGroupSendMessageToString :: ErrGroupSendMessage -> IO ByteString
toxErrGroupSendMessageToString value = Raw.toxErrGroupSendMessageToString value >>= BS.packCString
toxErrGroupSendPrivateMessageToString :: ErrGroupSendPrivateMessage -> IO ByteString
toxErrGroupSendPrivateMessageToString value = Raw.toxErrGroupSendPrivateMessageToString value >>= BS.packCString
toxErrGroupSendCustomPacketToString :: ErrGroupSendCustomPacket -> IO ByteString
toxErrGroupSendCustomPacketToString value = Raw.toxErrGroupSendCustomPacketToString value >>= BS.packCString
toxErrGroupSendCustomPrivatePacketToString :: ErrGroupSendCustomPrivatePacket -> IO ByteString
toxErrGroupSendCustomPrivatePacketToString value = Raw.toxErrGroupSendCustomPrivatePacketToString value >>= BS.packCString
toxErrGroupInviteFriendToString :: ErrGroupInviteFriend -> IO ByteString
toxErrGroupInviteFriendToString value = Raw.toxErrGroupInviteFriendToString value >>= BS.packCString
toxErrGroupInviteAcceptToString :: ErrGroupInviteAccept -> IO ByteString
toxErrGroupInviteAcceptToString value = Raw.toxErrGroupInviteAcceptToString value >>= BS.packCString
toxErrGroupSetPasswordToString :: ErrGroupSetPassword -> IO ByteString
toxErrGroupSetPasswordToString value = Raw.toxErrGroupSetPasswordToString value >>= BS.packCString
toxErrGroupSetTopicLockToString :: ErrGroupSetTopicLock -> IO ByteString
toxErrGroupSetTopicLockToString value = Raw.toxErrGroupSetTopicLockToString value >>= BS.packCString
toxErrGroupSetVoiceStateToString :: ErrGroupSetVoiceState -> IO ByteString
toxErrGroupSetVoiceStateToString value = Raw.toxErrGroupSetVoiceStateToString value >>= BS.packCString
toxErrGroupSetPrivacyStateToString :: ErrGroupSetPrivacyState -> IO ByteString
toxErrGroupSetPrivacyStateToString value = Raw.toxErrGroupSetPrivacyStateToString value >>= BS.packCString
toxErrGroupSetPeerLimitToString :: ErrGroupSetPeerLimit -> IO ByteString
toxErrGroupSetPeerLimitToString value = Raw.toxErrGroupSetPeerLimitToString value >>= BS.packCString
toxErrGroupSetIgnoreToString :: ErrGroupSetIgnore -> IO ByteString
toxErrGroupSetIgnoreToString value = Raw.toxErrGroupSetIgnoreToString value >>= BS.packCString
toxErrGroupSetRoleToString :: ErrGroupSetRole -> IO ByteString
toxErrGroupSetRoleToString value = Raw.toxErrGroupSetRoleToString value >>= BS.packCString
toxErrGroupKickPeerToString :: ErrGroupKickPeer -> IO ByteString
toxErrGroupKickPeerToString value = Raw.toxErrGroupKickPeerToString value >>= BS.packCString
toxLogLevelToString :: LogLevel -> IO ByteString
toxLogLevelToString value = Raw.toxLogLevelToString value >>= BS.packCString
toxProxyTypeToString :: ProxyType -> IO ByteString
toxProxyTypeToString value = Raw.toxProxyTypeToString value >>= BS.packCString
toxSavedataTypeToString :: SavedataType -> IO ByteString
toxSavedataTypeToString value = Raw.toxSavedataTypeToString value >>= BS.packCString
toxErrOptionsNewToString :: ErrOptionsNew -> IO ByteString
toxErrOptionsNewToString value = Raw.toxErrOptionsNewToString value >>= BS.packCString
toxCallbackSelfConnectionStatus :: ToxPtr -> FunPtr SelfConnectionStatusCb -> IO ()
toxCallbackSelfConnectionStatus this callback = Raw.toxCallbackSelfConnectionStatus this (castFunPtrToPtr callback)
