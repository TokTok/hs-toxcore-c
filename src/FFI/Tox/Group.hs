{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.Group where

import           Control.Monad         ((>=>))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Int              (Int32)
import           Data.MessagePack      (MessagePack)
import           Data.Word             (Word16, Word32, Word64, Word8)
import           FFI.Tox.Core
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

toxGroupMaxTopicLength :: ToxPtr -> IO Word32
toxGroupMaxTopicLength this = fmap fromIntegral Raw.toxGroupMaxTopicLength
toxGroupMaxPartLength :: ToxPtr -> IO Word32
toxGroupMaxPartLength this = fmap fromIntegral Raw.toxGroupMaxPartLength
toxGroupMaxMessageLength :: ToxPtr -> IO Word32
toxGroupMaxMessageLength this = fmap fromIntegral Raw.toxGroupMaxMessageLength
toxGroupMaxCustomLossyPacketLength :: ToxPtr -> IO Word32
toxGroupMaxCustomLossyPacketLength this = fmap fromIntegral Raw.toxGroupMaxCustomLossyPacketLength
toxGroupMaxCustomLosslessPacketLength :: ToxPtr -> IO Word32
toxGroupMaxCustomLosslessPacketLength this = fmap fromIntegral Raw.toxGroupMaxCustomLosslessPacketLength
toxGroupMaxGroupNameLength :: ToxPtr -> IO Word32
toxGroupMaxGroupNameLength this = fmap fromIntegral Raw.toxGroupMaxGroupNameLength
toxGroupMaxPasswordSize :: ToxPtr -> IO Word32
toxGroupMaxPasswordSize this = fmap fromIntegral Raw.toxGroupMaxPasswordSize
toxGroupChatIdSize :: ToxPtr -> IO Word32
toxGroupChatIdSize this = return FFI.Tox.Types.toxGroupChatIdSize_Const
toxGroupPrivacyStateToString :: ToxPtr -> GroupPrivacyState -> IO ByteString
toxGroupPrivacyStateToString this value = Raw.toxGroupPrivacyStateToString value >>= BS.packCString
toxGroupTopicLockToString :: ToxPtr -> GroupTopicLock -> IO ByteString
toxGroupTopicLockToString this value = Raw.toxGroupTopicLockToString value >>= BS.packCString
toxGroupVoiceStateToString :: ToxPtr -> GroupVoiceState -> IO ByteString
toxGroupVoiceStateToString this value = Raw.toxGroupVoiceStateToString value >>= BS.packCString
toxGroupRoleToString :: ToxPtr -> GroupRole -> IO ByteString
toxGroupRoleToString this value = Raw.toxGroupRoleToString value >>= BS.packCString
toxGroupNew :: ToxPtr -> GroupPrivacyState -> ByteString -> ByteString -> IO (Either ErrGroupNew GroupNumber)
toxGroupNew this privacyState groupName name = BS.useAsCStringLen groupName (\(ptr1, len1) -> BS.useAsCStringLen name (\(ptr2, len2) -> fmap (fmap GroupNumber) (callErrFun (\errPtr -> Raw.toxGroupNew this privacyState (castPtr ptr1) (fromIntegral len1) (castPtr ptr2) (fromIntegral len2) (castPtr errPtr)))))
toxGroupJoin :: ToxPtr -> ByteString -> ByteString -> ByteString -> IO (Either ErrGroupJoin GroupNumber)
toxGroupJoin this chatId name password = BS.useAsCString chatId (\ptr0 -> BS.useAsCStringLen name (\(ptr1, len1) -> BS.useAsCStringLen password (\(ptr2, len2) -> fmap (fmap GroupNumber) (callErrFun (\errPtr -> Raw.toxGroupJoin this (castPtr ptr0) (castPtr ptr1) (fromIntegral len1) (castPtr ptr2) (fromIntegral len2) (castPtr errPtr))))))
toxGroupIsConnected :: ToxPtr -> GroupNumber -> IO (Either ErrGroupIsConnected Bool)
toxGroupIsConnected this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupIsConnected this id1 (castPtr errPtr))
toxGroupDisconnect :: ToxPtr -> GroupNumber -> IO (Either ErrGroupDisconnect Bool)
toxGroupDisconnect this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupDisconnect this id1 (castPtr errPtr))
toxGroupLeave :: ToxPtr -> GroupNumber -> ByteString -> IO (Either ErrGroupLeave Bool)
toxGroupLeave this (GroupNumber id1) partMessage = BS.useAsCStringLen partMessage (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxGroupLeave this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxGroupSelfSetName :: ToxPtr -> GroupNumber -> ByteString -> IO (Either ErrGroupSelfNameSet Bool)
toxGroupSelfSetName this (GroupNumber id1) name = BS.useAsCStringLen name (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxGroupSelfSetName this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxGroupSelfGetNameSize :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery CSize)
toxGroupSelfGetNameSize this (GroupNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupSelfGetNameSize this id1 (castPtr errPtr)))
toxGroupSelfGetName :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery ByteString)
toxGroupSelfGetName this (GroupNumber id1) = do { size <- Raw.toxGroupSelfGetNameSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupSelfGetName this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupSelfSetStatus :: ToxPtr -> GroupNumber -> UserStatus -> IO (Either ErrGroupSelfStatusSet Bool)
toxGroupSelfSetStatus this (GroupNumber id1) status = callErrFun (\errPtr -> Raw.toxGroupSelfSetStatus this id1 status (castPtr errPtr))
toxGroupSelfGetStatus :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery UserStatus)
toxGroupSelfGetStatus this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupSelfGetStatus this id1 (castPtr errPtr))
toxGroupSelfGetRole :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery GroupRole)
toxGroupSelfGetRole this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupSelfGetRole this id1 (castPtr errPtr))
toxGroupSelfGetPeerId :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery GroupPeerNumber)
toxGroupSelfGetPeerId this (GroupNumber id1) = fmap (fmap GroupPeerNumber) (callErrFun (\errPtr -> Raw.toxGroupSelfGetPeerId this id1 (castPtr errPtr)))
toxGroupSelfGetPublicKey :: ToxPtr -> GroupNumber -> IO (Either ErrGroupSelfQuery ByteString)
toxGroupSelfGetPublicKey this (GroupNumber id1) = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupSelfGetPublicKey this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupSetTopic :: ToxPtr -> GroupNumber -> ByteString -> IO (Either ErrGroupTopicSet Bool)
toxGroupSetTopic this (GroupNumber id1) topic = BS.useAsCStringLen topic (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxGroupSetTopic this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxGroupGetTopicSize :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery CSize)
toxGroupGetTopicSize this (GroupNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupGetTopicSize this id1 (castPtr errPtr)))
toxGroupGetTopic :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery ByteString)
toxGroupGetTopic this (GroupNumber id1) = do { size <- Raw.toxGroupGetTopicSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupGetTopic this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupGetNameSize :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery CSize)
toxGroupGetNameSize this (GroupNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupGetNameSize this id1 (castPtr errPtr)))
toxGroupGetName :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery ByteString)
toxGroupGetName this (GroupNumber id1) = do { size <- Raw.toxGroupGetNameSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupGetName this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupGetChatId :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery ByteString)
toxGroupGetChatId this (GroupNumber id1) = do { let { size  = FFI.Tox.Types.toxGroupChatIdSize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupGetChatId this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupGetNumberGroups :: ToxPtr -> IO Word32
toxGroupGetNumberGroups this = fmap fromIntegral (Raw.toxGroupGetNumberGroups this)
toxGroupGetPrivacyState :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery GroupPrivacyState)
toxGroupGetPrivacyState this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupGetPrivacyState this id1 (castPtr errPtr))
toxGroupGetVoiceState :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery GroupVoiceState)
toxGroupGetVoiceState this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupGetVoiceState this id1 (castPtr errPtr))
toxGroupGetTopicLock :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery GroupTopicLock)
toxGroupGetTopicLock this (GroupNumber id1) = callErrFun (\errPtr -> Raw.toxGroupGetTopicLock this id1 (castPtr errPtr))
toxGroupGetPeerLimit :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery Word16)
toxGroupGetPeerLimit this (GroupNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupGetPeerLimit this id1 (castPtr errPtr)))
toxGroupGetPasswordSize :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery CSize)
toxGroupGetPasswordSize this (GroupNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupGetPasswordSize this id1 (castPtr errPtr)))
toxGroupGetPassword :: ToxPtr -> GroupNumber -> IO (Either ErrGroupStateQuery ByteString)
toxGroupGetPassword this (GroupNumber id1) = do { size <- Raw.toxGroupGetPasswordSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupGetPassword this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupSendMessage :: ToxPtr -> GroupNumber -> MessageType -> ByteString -> IO (Either ErrGroupSendMessage GroupMessageId)
toxGroupSendMessage this (GroupNumber id1) messageType message = BS.useAsCStringLen message (\(ptr1, len1) -> fmap (fmap GroupMessageId) (callErrFun (\errPtr -> Raw.toxGroupSendMessage this id1 messageType (castPtr ptr1) (fromIntegral len1) (castPtr errPtr))))
toxGroupSendPrivateMessage :: ToxPtr -> GroupNumber -> GroupPeerNumber -> MessageType -> ByteString -> IO (Either ErrGroupSendPrivateMessage GroupMessageId)
toxGroupSendPrivateMessage this (GroupNumber id1) (GroupPeerNumber peerId) messageType message = BS.useAsCStringLen message (\(ptr2, len2) -> fmap (fmap GroupMessageId) (callErrFun (\errPtr -> Raw.toxGroupSendPrivateMessage this id1 peerId messageType (castPtr ptr2) (fromIntegral len2) (castPtr errPtr))))
toxGroupSendCustomPacket :: ToxPtr -> GroupNumber -> Bool -> ByteString -> IO (Either ErrGroupSendCustomPacket Bool)
toxGroupSendCustomPacket this (GroupNumber id1) lossless data_ = BS.useAsCStringLen data_ (\(ptr1, len1) -> callErrFun (\errPtr -> Raw.toxGroupSendCustomPacket this id1 lossless (castPtr ptr1) (fromIntegral len1) (castPtr errPtr)))
toxGroupSendCustomPrivatePacket :: ToxPtr -> GroupNumber -> GroupPeerNumber -> Bool -> ByteString -> IO (Either ErrGroupSendCustomPrivatePacket Bool)
toxGroupSendCustomPrivatePacket this (GroupNumber id1) (GroupPeerNumber peerId) lossless data_ = BS.useAsCStringLen data_ (\(ptr2, len2) -> callErrFun (\errPtr -> Raw.toxGroupSendCustomPrivatePacket this id1 peerId lossless (castPtr ptr2) (fromIntegral len2) (castPtr errPtr)))
toxGroupInviteFriend :: ToxPtr -> GroupNumber -> FriendNumber -> IO (Either ErrGroupInviteFriend Bool)
toxGroupInviteFriend this (GroupNumber id1) (FriendNumber friendNumber) = callErrFun (\errPtr -> Raw.toxGroupInviteFriend this id1 friendNumber (castPtr errPtr))
toxGroupInviteAccept :: ToxPtr -> FriendNumber -> ByteString -> ByteString -> ByteString -> IO (Either ErrGroupInviteAccept GroupNumber)
toxGroupInviteAccept this (FriendNumber friendNumber) inviteData name password = BS.useAsCStringLen inviteData (\(ptr1, len1) -> BS.useAsCStringLen name (\(ptr2, len2) -> BS.useAsCStringLen password (\(ptr3, len3) -> fmap (fmap GroupNumber) (callErrFun (\errPtr -> Raw.toxGroupInviteAccept this friendNumber (castPtr ptr1) (fromIntegral len1) (castPtr ptr2) (fromIntegral len2) (castPtr ptr3) (fromIntegral len3) (castPtr errPtr))))))
toxGroupExitTypeToString :: ToxPtr -> GroupExitType -> IO ByteString
toxGroupExitTypeToString this value = Raw.toxGroupExitTypeToString value >>= BS.packCString
toxGroupJoinFailToString :: ToxPtr -> GroupJoinFail -> IO ByteString
toxGroupJoinFailToString this value = Raw.toxGroupJoinFailToString value >>= BS.packCString
toxGroupSetPassword :: ToxPtr -> GroupNumber -> ByteString -> IO (Either ErrGroupSetPassword Bool)
toxGroupSetPassword this (GroupNumber id1) password = BS.useAsCStringLen password (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxGroupSetPassword this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxGroupSetTopicLock :: ToxPtr -> GroupNumber -> GroupTopicLock -> IO (Either ErrGroupSetTopicLock Bool)
toxGroupSetTopicLock this (GroupNumber id1) topicLock = callErrFun (\errPtr -> Raw.toxGroupSetTopicLock this id1 topicLock (castPtr errPtr))
toxGroupSetVoiceState :: ToxPtr -> GroupNumber -> GroupVoiceState -> IO (Either ErrGroupSetVoiceState Bool)
toxGroupSetVoiceState this (GroupNumber id1) voiceState = callErrFun (\errPtr -> Raw.toxGroupSetVoiceState this id1 voiceState (castPtr errPtr))
toxGroupSetPrivacyState :: ToxPtr -> GroupNumber -> GroupPrivacyState -> IO (Either ErrGroupSetPrivacyState Bool)
toxGroupSetPrivacyState this (GroupNumber id1) privacyState = callErrFun (\errPtr -> Raw.toxGroupSetPrivacyState this id1 privacyState (castPtr errPtr))
toxGroupSetPeerLimit :: ToxPtr -> GroupNumber -> Word16 -> IO (Either ErrGroupSetPeerLimit Bool)
toxGroupSetPeerLimit this (GroupNumber id1) peerLimit = callErrFun (\errPtr -> Raw.toxGroupSetPeerLimit this id1 (fromIntegral peerLimit) (castPtr errPtr))
toxGroupSetIgnore :: ToxPtr -> GroupNumber -> GroupPeerNumber -> Bool -> IO (Either ErrGroupSetIgnore Bool)
toxGroupSetIgnore this (GroupNumber id1) (GroupPeerNumber peerId) ignore = callErrFun (\errPtr -> Raw.toxGroupSetIgnore this id1 peerId ignore (castPtr errPtr))
toxGroupSetRole :: ToxPtr -> GroupNumber -> GroupPeerNumber -> GroupRole -> IO (Either ErrGroupSetRole Bool)
toxGroupSetRole this (GroupNumber id1) (GroupPeerNumber peerId) role = callErrFun (\errPtr -> Raw.toxGroupSetRole this id1 peerId role (castPtr errPtr))
toxGroupKickPeer :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupKickPeer Bool)
toxGroupKickPeer this (GroupNumber id1) (GroupPeerNumber peerId) = callErrFun (\errPtr -> Raw.toxGroupKickPeer this id1 peerId (castPtr errPtr))
toxGroupModEventToString :: GroupModEvent -> IO ByteString
toxGroupModEventToString value = Raw.toxGroupModEventToString value >>= BS.packCString
toxCallbackGroupInvite :: ToxPtr -> FunPtr GroupInviteCb -> IO ()
toxCallbackGroupInvite this callback = Raw.toxCallbackGroupInvite this (castFunPtrToPtr callback)
toxCallbackGroupMessage :: ToxPtr -> FunPtr GroupMessageCb -> IO ()
toxCallbackGroupMessage this callback = Raw.toxCallbackGroupMessage this (castFunPtrToPtr callback)
toxCallbackGroupPrivateMessage :: ToxPtr -> FunPtr GroupPrivateMessageCb -> IO ()
toxCallbackGroupPrivateMessage this callback = Raw.toxCallbackGroupPrivateMessage this (castFunPtrToPtr callback)
toxCallbackGroupCustomPacket :: ToxPtr -> FunPtr GroupCustomPacketCb -> IO ()
toxCallbackGroupCustomPacket this callback = Raw.toxCallbackGroupCustomPacket this (castFunPtrToPtr callback)
toxCallbackGroupCustomPrivatePacket :: ToxPtr -> FunPtr GroupCustomPrivatePacketCb -> IO ()
toxCallbackGroupCustomPrivatePacket this callback = Raw.toxCallbackGroupCustomPrivatePacket this (castFunPtrToPtr callback)
toxCallbackGroupPeerJoin :: ToxPtr -> FunPtr GroupPeerJoinCb -> IO ()
toxCallbackGroupPeerJoin this callback = Raw.toxCallbackGroupPeerJoin this (castFunPtrToPtr callback)
toxCallbackGroupPeerExit :: ToxPtr -> FunPtr GroupPeerExitCb -> IO ()
toxCallbackGroupPeerExit this callback = Raw.toxCallbackGroupPeerExit this (castFunPtrToPtr callback)
toxCallbackGroupSelfJoin :: ToxPtr -> FunPtr GroupSelfJoinCb -> IO ()
toxCallbackGroupSelfJoin this callback = Raw.toxCallbackGroupSelfJoin this (castFunPtrToPtr callback)
toxCallbackGroupJoinFail :: ToxPtr -> FunPtr GroupJoinFailCb -> IO ()
toxCallbackGroupJoinFail this callback = Raw.toxCallbackGroupJoinFail this (castFunPtrToPtr callback)
toxCallbackGroupModeration :: ToxPtr -> FunPtr GroupModerationCb -> IO ()
toxCallbackGroupModeration this callback = Raw.toxCallbackGroupModeration this (castFunPtrToPtr callback)
toxCallbackGroupPeerName :: ToxPtr -> FunPtr GroupPeerNameCb -> IO ()
toxCallbackGroupPeerName this callback = Raw.toxCallbackGroupPeerName this (castFunPtrToPtr callback)
toxCallbackGroupPeerStatus :: ToxPtr -> FunPtr GroupPeerStatusCb -> IO ()
toxCallbackGroupPeerStatus this callback = Raw.toxCallbackGroupPeerStatus this (castFunPtrToPtr callback)
toxCallbackGroupTopic :: ToxPtr -> FunPtr GroupTopicCb -> IO ()
toxCallbackGroupTopic this callback = Raw.toxCallbackGroupTopic this (castFunPtrToPtr callback)
toxCallbackGroupPrivacyState :: ToxPtr -> FunPtr GroupPrivacyStateCb -> IO ()
toxCallbackGroupPrivacyState this callback = Raw.toxCallbackGroupPrivacyState this (castFunPtrToPtr callback)
toxCallbackGroupVoiceState :: ToxPtr -> FunPtr GroupVoiceStateCb -> IO ()
toxCallbackGroupVoiceState this callback = Raw.toxCallbackGroupVoiceState this (castFunPtrToPtr callback)
toxCallbackGroupTopicLock :: ToxPtr -> FunPtr GroupTopicLockCb -> IO ()
toxCallbackGroupTopicLock this callback = Raw.toxCallbackGroupTopicLock this (castFunPtrToPtr callback)
toxCallbackGroupPeerLimit :: ToxPtr -> FunPtr GroupPeerLimitCb -> IO ()
toxCallbackGroupPeerLimit this callback = Raw.toxCallbackGroupPeerLimit this (castFunPtrToPtr callback)
toxCallbackGroupPassword :: ToxPtr -> FunPtr GroupPasswordCb -> IO ()
toxCallbackGroupPassword this callback = Raw.toxCallbackGroupPassword this (castFunPtrToPtr callback)
