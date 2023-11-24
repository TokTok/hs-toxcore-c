-- bazel-out/k8-fastbuild/bin/c-toxcore/tox/tox.h
{-# LANGUAGE DeriveGeneric #-}
module FFI.Tox.Tox where

import           Data.MessagePack          (MessagePack)
import           Data.Word                 (Word16, Word32, Word64)
import           Foreign.C.Enum            (CEnum (..), CErr)
import           Foreign.C.String          (CString)
import           Foreign.C.Types           (CInt (..), CSize (..))
import           Foreign.Ptr               (FunPtr, Ptr)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (..),
                                            arbitraryBoundedEnum)

data ToxStruct
type ToxPtr = Ptr ToxStruct
foreign import ccall tox_version_major :: Word32
foreign import ccall tox_version_minor :: Word32
foreign import ccall tox_version_patch :: Word32
foreign import ccall tox_version_is_compatible :: Word32 -> Word32 -> Word32 -> IO Bool
foreign import ccall tox_public_key_size :: Word32
foreign import ccall tox_secret_key_size :: Word32
foreign import ccall tox_conference_uid_size :: Word32
foreign import ccall tox_conference_id_size :: Word32
foreign import ccall tox_nospam_size :: Word32
foreign import ccall tox_address_size :: Word32
foreign import ccall tox_max_name_length :: Word32
foreign import ccall tox_max_status_message_length :: Word32
foreign import ccall tox_max_friend_request_length :: Word32
foreign import ccall tox_max_message_length :: Word32
foreign import ccall tox_max_custom_packet_size :: Word32
foreign import ccall tox_hash_length :: Word32
foreign import ccall tox_file_id_length :: Word32
foreign import ccall tox_max_filename_length :: Word32
foreign import ccall tox_max_hostname_length :: Word32
data UserStatus
    = UserStatusNone
    | UserStatusAway
    | UserStatusBusy
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack UserStatus
instance Arbitrary UserStatus where arbitrary = arbitraryBoundedEnum
data MessageType
    = MessageTypeNormal
    | MessageTypeAction
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack MessageType
instance Arbitrary MessageType where arbitrary = arbitraryBoundedEnum
data ProxyType
    = ProxyTypeNone
    | ProxyTypeHttp
    | ProxyTypeSocks5
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ProxyType
instance Arbitrary ProxyType where arbitrary = arbitraryBoundedEnum
data SavedataType
    = SavedataTypeNone
    | SavedataTypeToxSave
    | SavedataTypeSecretKey
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack SavedataType
instance Arbitrary SavedataType where arbitrary = arbitraryBoundedEnum
data LogLevel
    = LogLevelTrace
    | LogLevelDebug
    | LogLevelInfo
    | LogLevelWarning
    | LogLevelError
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack LogLevel
instance Arbitrary LogLevel where arbitrary = arbitraryBoundedEnum
type LogCb = ToxPtr -> CEnum LogLevel -> CString -> Word32 -> CString -> CString -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapLogCb :: LogCb -> IO (FunPtr LogCb)
data SystemStruct
type SystemPtr = Ptr SystemStruct
data OptionsStruct
type OptionsPtr = Ptr OptionsStruct
foreign import ccall tox_options_get_ipv6_enabled :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_ipv6_enabled :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_udp_enabled :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_udp_enabled :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_local_discovery_enabled :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_local_discovery_enabled :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_dht_announcements_enabled :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_dht_announcements_enabled :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_proxy_type :: OptionsPtr -> IO (CEnum ProxyType)
foreign import ccall tox_options_set_proxy_type :: OptionsPtr -> CEnum ProxyType -> IO ()
foreign import ccall tox_options_get_proxy_host :: OptionsPtr -> IO CString
foreign import ccall tox_options_set_proxy_host :: OptionsPtr -> CString -> IO ()
foreign import ccall tox_options_get_proxy_port :: OptionsPtr -> IO Word16
foreign import ccall tox_options_set_proxy_port :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_get_start_port :: OptionsPtr -> IO Word16
foreign import ccall tox_options_set_start_port :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_get_end_port :: OptionsPtr -> IO Word16
foreign import ccall tox_options_set_end_port :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_get_tcp_port :: OptionsPtr -> IO Word16
foreign import ccall tox_options_set_tcp_port :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_get_hole_punching_enabled :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_hole_punching_enabled :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_savedata_type :: OptionsPtr -> IO (CEnum SavedataType)
foreign import ccall tox_options_set_savedata_type :: OptionsPtr -> CEnum SavedataType -> IO ()
foreign import ccall tox_options_get_savedata_data :: OptionsPtr -> IO CString
foreign import ccall tox_options_set_savedata_data :: OptionsPtr -> CString -> CSize -> IO ()
foreign import ccall tox_options_get_savedata_length :: OptionsPtr -> IO CSize
foreign import ccall tox_options_set_savedata_length :: OptionsPtr -> CSize -> IO ()
foreign import ccall tox_options_get_log_callback :: OptionsPtr -> IO (FunPtr LogCb)
foreign import ccall tox_options_set_log_callback :: OptionsPtr -> FunPtr LogCb -> IO ()
foreign import ccall tox_options_get_log_user_data :: OptionsPtr -> IO (Ptr ())
foreign import ccall tox_options_set_log_user_data :: OptionsPtr -> Ptr () -> IO ()
foreign import ccall tox_options_get_experimental_thread_safety :: OptionsPtr -> IO Bool
foreign import ccall tox_options_set_experimental_thread_safety :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_get_operating_system :: OptionsPtr -> IO SystemPtr
foreign import ccall tox_options_set_operating_system :: OptionsPtr -> SystemPtr -> IO ()
foreign import ccall tox_options_default :: OptionsPtr -> IO ()
data ErrOptionsNew
    = ErrOptionsNewMalloc
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrOptionsNew
instance Arbitrary ErrOptionsNew where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_options_new :: CErr ErrOptionsNew -> IO OptionsPtr
foreign import ccall tox_options_free :: OptionsPtr -> IO ()
data ErrNew
    = ErrNewNull
    | ErrNewMalloc
    | ErrNewPortAlloc
    | ErrNewProxyBadType
    | ErrNewProxyBadHost
    | ErrNewProxyBadPort
    | ErrNewProxyNotFound
    | ErrNewLoadEncrypted
    | ErrNewLoadBadFormat
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrNew
instance Arbitrary ErrNew where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_new :: OptionsPtr -> CErr ErrNew -> IO ToxPtr
foreign import ccall tox_kill :: ToxPtr -> IO ()
foreign import ccall tox_get_system :: ToxPtr -> IO SystemPtr
foreign import ccall tox_get_savedata_size :: ToxPtr -> IO CSize
foreign import ccall tox_get_savedata :: ToxPtr -> CString -> IO ()
data ErrBootstrap
    = ErrBootstrapNull
    | ErrBootstrapBadHost
    | ErrBootstrapBadPort
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrBootstrap
instance Arbitrary ErrBootstrap where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_bootstrap :: ToxPtr -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO Bool
foreign import ccall tox_add_tcp_relay :: ToxPtr -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO Bool
data Connection
    = ConnectionNone
    | ConnectionTcp
    | ConnectionUdp
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack Connection
instance Arbitrary Connection where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_self_get_connection_status :: ToxPtr -> IO (CEnum Connection)
type SelfConnectionStatusCb = ToxPtr -> CEnum Connection -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapSelfConnectionStatusCb :: SelfConnectionStatusCb -> IO (FunPtr SelfConnectionStatusCb)
foreign import ccall tox_callback_self_connection_status :: ToxPtr -> FunPtr SelfConnectionStatusCb -> IO ()
foreign import ccall tox_iteration_interval :: ToxPtr -> IO Word32
foreign import ccall tox_iterate :: ToxPtr -> Ptr () -> IO ()
foreign import ccall tox_self_get_address :: ToxPtr -> CString -> IO ()
foreign import ccall tox_self_set_nospam :: ToxPtr -> Word32 -> IO ()
foreign import ccall tox_self_get_nospam :: ToxPtr -> IO Word32
foreign import ccall tox_self_get_public_key :: ToxPtr -> CString -> IO ()
foreign import ccall tox_self_get_secret_key :: ToxPtr -> CString -> IO ()
data ErrSetInfo
    = ErrSetInfoNull
    | ErrSetInfoTooLong
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrSetInfo
instance Arbitrary ErrSetInfo where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_self_set_name :: ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO Bool
foreign import ccall tox_self_get_name_size :: ToxPtr -> IO CSize
foreign import ccall tox_self_get_name :: ToxPtr -> CString -> IO ()
foreign import ccall tox_self_set_status_message :: ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO Bool
foreign import ccall tox_self_get_status_message_size :: ToxPtr -> IO CSize
foreign import ccall tox_self_get_status_message :: ToxPtr -> CString -> IO ()
foreign import ccall tox_self_set_status :: ToxPtr -> CEnum UserStatus -> IO ()
foreign import ccall tox_self_get_status :: ToxPtr -> IO (CEnum UserStatus)
data ErrFriendAdd
    = ErrFriendAddNull
    | ErrFriendAddTooLong
    | ErrFriendAddNoMessage
    | ErrFriendAddOwnKey
    | ErrFriendAddAlreadySent
    | ErrFriendAddBadChecksum
    | ErrFriendAddSetNewNospam
    | ErrFriendAddMalloc
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendAdd
instance Arbitrary ErrFriendAdd where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_add :: ToxPtr -> CString -> CString -> CSize -> CErr ErrFriendAdd -> IO Word32
foreign import ccall tox_friend_add_norequest :: ToxPtr -> CString -> CErr ErrFriendAdd -> IO Word32
data ErrFriendDelete
    = ErrFriendDeleteFriendNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendDelete
instance Arbitrary ErrFriendDelete where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_delete :: ToxPtr -> Word32 -> CErr ErrFriendDelete -> IO Bool
data ErrFriendByPublicKey
    = ErrFriendByPublicKeyNull
    | ErrFriendByPublicKeyNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendByPublicKey
instance Arbitrary ErrFriendByPublicKey where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_by_public_key :: ToxPtr -> CString -> CErr ErrFriendByPublicKey -> IO Word32
foreign import ccall tox_friend_exists :: ToxPtr -> Word32 -> IO Bool
foreign import ccall tox_self_get_friend_list_size :: ToxPtr -> IO CSize
foreign import ccall tox_self_get_friend_list :: ToxPtr -> Ptr Word32 -> IO ()
data ErrFriendGetPublicKey
    = ErrFriendGetPublicKeyFriendNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendGetPublicKey
instance Arbitrary ErrFriendGetPublicKey where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_get_public_key :: ToxPtr -> Word32 -> CString -> CErr ErrFriendGetPublicKey -> IO Bool
data ErrFriendGetLastOnline
    = ErrFriendGetLastOnlineFriendNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendGetLastOnline
instance Arbitrary ErrFriendGetLastOnline where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_get_last_online :: ToxPtr -> Word32 -> CErr ErrFriendGetLastOnline -> IO Word64
data ErrFriendQuery
    = ErrFriendQueryNull
    | ErrFriendQueryFriendNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendQuery
instance Arbitrary ErrFriendQuery where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_get_name_size :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO CSize
foreign import ccall tox_friend_get_name :: ToxPtr -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool
type FriendNameCb = ToxPtr -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendNameCb :: FriendNameCb -> IO (FunPtr FriendNameCb)
foreign import ccall tox_callback_friend_name :: ToxPtr -> FunPtr FriendNameCb -> IO ()
foreign import ccall tox_friend_get_status_message_size :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO CSize
foreign import ccall tox_friend_get_status_message :: ToxPtr -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool
type FriendStatusMessageCb = ToxPtr -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendStatusMessageCb :: FriendStatusMessageCb -> IO (FunPtr FriendStatusMessageCb)
foreign import ccall tox_callback_friend_status_message :: ToxPtr -> FunPtr FriendStatusMessageCb -> IO ()
foreign import ccall tox_friend_get_status :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO (CEnum UserStatus)
type FriendStatusCb = ToxPtr -> Word32 -> CEnum UserStatus -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendStatusCb :: FriendStatusCb -> IO (FunPtr FriendStatusCb)
foreign import ccall tox_callback_friend_status :: ToxPtr -> FunPtr FriendStatusCb -> IO ()
foreign import ccall tox_friend_get_connection_status :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO (CEnum Connection)
type FriendConnectionStatusCb = ToxPtr -> Word32 -> CEnum Connection -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendConnectionStatusCb :: FriendConnectionStatusCb -> IO (FunPtr FriendConnectionStatusCb)
foreign import ccall tox_callback_friend_connection_status :: ToxPtr -> FunPtr FriendConnectionStatusCb -> IO ()
foreign import ccall tox_friend_get_typing :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO Bool
type FriendTypingCb = ToxPtr -> Word32 -> Bool -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendTypingCb :: FriendTypingCb -> IO (FunPtr FriendTypingCb)
foreign import ccall tox_callback_friend_typing :: ToxPtr -> FunPtr FriendTypingCb -> IO ()
data ErrSetTyping
    = ErrSetTypingFriendNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrSetTyping
instance Arbitrary ErrSetTyping where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_self_set_typing :: ToxPtr -> Word32 -> Bool -> CErr ErrSetTyping -> IO Bool
data ErrFriendSendMessage
    = ErrFriendSendMessageNull
    | ErrFriendSendMessageFriendNotFound
    | ErrFriendSendMessageFriendNotConnected
    | ErrFriendSendMessageSendq
    | ErrFriendSendMessageTooLong
    | ErrFriendSendMessageEmpty
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendSendMessage
instance Arbitrary ErrFriendSendMessage where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_send_message :: ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrFriendSendMessage -> IO Word32
type FriendReadReceiptCb = ToxPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendReadReceiptCb :: FriendReadReceiptCb -> IO (FunPtr FriendReadReceiptCb)
foreign import ccall tox_callback_friend_read_receipt :: ToxPtr -> FunPtr FriendReadReceiptCb -> IO ()
type FriendRequestCb = ToxPtr -> CString -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendRequestCb :: FriendRequestCb -> IO (FunPtr FriendRequestCb)
foreign import ccall tox_callback_friend_request :: ToxPtr -> FunPtr FriendRequestCb -> IO ()
type FriendMessageCb = ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendMessageCb :: FriendMessageCb -> IO (FunPtr FriendMessageCb)
foreign import ccall tox_callback_friend_message :: ToxPtr -> FunPtr FriendMessageCb -> IO ()
foreign import ccall tox_hash :: CString -> CString -> CSize -> IO Bool
data FileKind
    = FileKindData
    | FileKindAvatar
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack FileKind
instance Arbitrary FileKind where arbitrary = arbitraryBoundedEnum
data FileControl
    = FileControlResume
    | FileControlPause
    | FileControlCancel
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack FileControl
instance Arbitrary FileControl where arbitrary = arbitraryBoundedEnum
data ErrFileControl
    = ErrFileControlFriendNotFound
    | ErrFileControlFriendNotConnected
    | ErrFileControlNotFound
    | ErrFileControlNotPaused
    | ErrFileControlDenied
    | ErrFileControlAlreadyPaused
    | ErrFileControlSendq
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFileControl
instance Arbitrary ErrFileControl where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_file_control :: ToxPtr -> Word32 -> Word32 -> CEnum FileControl -> CErr ErrFileControl -> IO Bool
type FileRecvControlCb = ToxPtr -> Word32 -> Word32 -> CEnum FileControl -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvControlCb :: FileRecvControlCb -> IO (FunPtr FileRecvControlCb)
foreign import ccall tox_callback_file_recv_control :: ToxPtr -> FunPtr FileRecvControlCb -> IO ()
data ErrFileSeek
    = ErrFileSeekFriendNotFound
    | ErrFileSeekFriendNotConnected
    | ErrFileSeekNotFound
    | ErrFileSeekDenied
    | ErrFileSeekInvalidPosition
    | ErrFileSeekSendq
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFileSeek
instance Arbitrary ErrFileSeek where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_file_seek :: ToxPtr -> Word32 -> Word32 -> Word64 -> CErr ErrFileSeek -> IO Bool
data ErrFileGet
    = ErrFileGetNull
    | ErrFileGetFriendNotFound
    | ErrFileGetNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFileGet
instance Arbitrary ErrFileGet where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_file_get_file_id :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrFileGet -> IO Bool
data ErrFileSend
    = ErrFileSendNull
    | ErrFileSendFriendNotFound
    | ErrFileSendFriendNotConnected
    | ErrFileSendNameTooLong
    | ErrFileSendTooMany
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFileSend
instance Arbitrary ErrFileSend where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_file_send :: ToxPtr -> Word32 -> Word32 -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32
data ErrFileSendChunk
    = ErrFileSendChunkNull
    | ErrFileSendChunkFriendNotFound
    | ErrFileSendChunkFriendNotConnected
    | ErrFileSendChunkNotFound
    | ErrFileSendChunkNotTransferring
    | ErrFileSendChunkInvalidLength
    | ErrFileSendChunkSendq
    | ErrFileSendChunkWrongPosition
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFileSendChunk
instance Arbitrary ErrFileSendChunk where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_file_send_chunk :: ToxPtr -> Word32 -> Word32 -> Word64 -> CString -> CSize -> CErr ErrFileSendChunk -> IO Bool
type FileChunkRequestCb = ToxPtr -> Word32 -> Word32 -> Word64 -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileChunkRequestCb :: FileChunkRequestCb -> IO (FunPtr FileChunkRequestCb)
foreign import ccall tox_callback_file_chunk_request :: ToxPtr -> FunPtr FileChunkRequestCb -> IO ()
type FileRecvCb = ToxPtr -> Word32 -> Word32 -> Word32 -> Word64 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvCb :: FileRecvCb -> IO (FunPtr FileRecvCb)
foreign import ccall tox_callback_file_recv :: ToxPtr -> FunPtr FileRecvCb -> IO ()
type FileRecvChunkCb = ToxPtr -> Word32 -> Word32 -> Word64 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFileRecvChunkCb :: FileRecvChunkCb -> IO (FunPtr FileRecvChunkCb)
foreign import ccall tox_callback_file_recv_chunk :: ToxPtr -> FunPtr FileRecvChunkCb -> IO ()
data ConferenceType
    = ConferenceTypeText
    | ConferenceTypeAv
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ConferenceType
instance Arbitrary ConferenceType where arbitrary = arbitraryBoundedEnum
type ConferenceInviteCb = ToxPtr -> Word32 -> CEnum ConferenceType -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceInviteCb :: ConferenceInviteCb -> IO (FunPtr ConferenceInviteCb)
foreign import ccall tox_callback_conference_invite :: ToxPtr -> FunPtr ConferenceInviteCb -> IO ()
type ConferenceConnectedCb = ToxPtr -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceConnectedCb :: ConferenceConnectedCb -> IO (FunPtr ConferenceConnectedCb)
foreign import ccall tox_callback_conference_connected :: ToxPtr -> FunPtr ConferenceConnectedCb -> IO ()
type ConferenceMessageCb = ToxPtr -> Word32 -> Word32 -> CEnum MessageType -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceMessageCb :: ConferenceMessageCb -> IO (FunPtr ConferenceMessageCb)
foreign import ccall tox_callback_conference_message :: ToxPtr -> FunPtr ConferenceMessageCb -> IO ()
type ConferenceTitleCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferenceTitleCb :: ConferenceTitleCb -> IO (FunPtr ConferenceTitleCb)
foreign import ccall tox_callback_conference_title :: ToxPtr -> FunPtr ConferenceTitleCb -> IO ()
type ConferencePeerNameCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferencePeerNameCb :: ConferencePeerNameCb -> IO (FunPtr ConferencePeerNameCb)
foreign import ccall tox_callback_conference_peer_name :: ToxPtr -> FunPtr ConferencePeerNameCb -> IO ()
type ConferencePeerListChangedCb = ToxPtr -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapConferencePeerListChangedCb :: ConferencePeerListChangedCb -> IO (FunPtr ConferencePeerListChangedCb)
foreign import ccall tox_callback_conference_peer_list_changed :: ToxPtr -> FunPtr ConferencePeerListChangedCb -> IO ()
data ErrConferenceNew
    = ErrConferenceNewInit
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceNew
instance Arbitrary ErrConferenceNew where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_new :: ToxPtr -> CErr ErrConferenceNew -> IO Word32
data ErrConferenceDelete
    = ErrConferenceDeleteConferenceNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceDelete
instance Arbitrary ErrConferenceDelete where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_delete :: ToxPtr -> Word32 -> CErr ErrConferenceDelete -> IO Bool
data ErrConferencePeerQuery
    = ErrConferencePeerQueryConferenceNotFound
    | ErrConferencePeerQueryPeerNotFound
    | ErrConferencePeerQueryNoConnection
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferencePeerQuery
instance Arbitrary ErrConferencePeerQuery where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_peer_count :: ToxPtr -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32
foreign import ccall tox_conference_peer_get_name_size :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO CSize
foreign import ccall tox_conference_peer_get_name :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
foreign import ccall tox_conference_peer_get_public_key :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
foreign import ccall tox_conference_peer_number_is_ours :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Bool
foreign import ccall tox_conference_offline_peer_count :: ToxPtr -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32
foreign import ccall tox_conference_offline_peer_get_name_size :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO CSize
foreign import ccall tox_conference_offline_peer_get_name :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
foreign import ccall tox_conference_offline_peer_get_public_key :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
foreign import ccall tox_conference_offline_peer_get_last_active :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Word64
data ErrConferenceSetMaxOffline
    = ErrConferenceSetMaxOfflineConferenceNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceSetMaxOffline
instance Arbitrary ErrConferenceSetMaxOffline where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_set_max_offline :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferenceSetMaxOffline -> IO Bool
data ErrConferenceInvite
    = ErrConferenceInviteConferenceNotFound
    | ErrConferenceInviteFailSend
    | ErrConferenceInviteNoConnection
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceInvite
instance Arbitrary ErrConferenceInvite where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_invite :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferenceInvite -> IO Bool
data ErrConferenceJoin
    = ErrConferenceJoinInvalidLength
    | ErrConferenceJoinWrongType
    | ErrConferenceJoinFriendNotFound
    | ErrConferenceJoinDuplicate
    | ErrConferenceJoinInitFail
    | ErrConferenceJoinFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceJoin
instance Arbitrary ErrConferenceJoin where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_join :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceJoin -> IO Word32
data ErrConferenceSendMessage
    = ErrConferenceSendMessageConferenceNotFound
    | ErrConferenceSendMessageTooLong
    | ErrConferenceSendMessageNoConnection
    | ErrConferenceSendMessageFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceSendMessage
instance Arbitrary ErrConferenceSendMessage where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_send_message :: ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrConferenceSendMessage -> IO Bool
data ErrConferenceTitle
    = ErrConferenceTitleConferenceNotFound
    | ErrConferenceTitleInvalidLength
    | ErrConferenceTitleFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceTitle
instance Arbitrary ErrConferenceTitle where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_get_title_size :: ToxPtr -> Word32 -> CErr ErrConferenceTitle -> IO CSize
foreign import ccall tox_conference_get_title :: ToxPtr -> Word32 -> CString -> CErr ErrConferenceTitle -> IO Bool
foreign import ccall tox_conference_set_title :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceTitle -> IO Bool
foreign import ccall tox_conference_get_chatlist_size :: ToxPtr -> IO CSize
foreign import ccall tox_conference_get_chatlist :: ToxPtr -> Ptr Word32 -> IO ()
data ErrConferenceGetType
    = ErrConferenceGetTypeConferenceNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceGetType
instance Arbitrary ErrConferenceGetType where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_get_type :: ToxPtr -> Word32 -> CErr ErrConferenceGetType -> IO (CEnum ConferenceType)
foreign import ccall tox_conference_get_id :: ToxPtr -> Word32 -> CString -> IO Bool
data ErrConferenceById
    = ErrConferenceByIdNull
    | ErrConferenceByIdNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceById
instance Arbitrary ErrConferenceById where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_by_id :: ToxPtr -> CString -> CErr ErrConferenceById -> IO Word32
foreign import ccall tox_conference_get_uid :: ToxPtr -> Word32 -> CString -> IO Bool
data ErrConferenceByUid
    = ErrConferenceByUidNull
    | ErrConferenceByUidNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrConferenceByUid
instance Arbitrary ErrConferenceByUid where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_conference_by_uid :: ToxPtr -> CString -> CErr ErrConferenceByUid -> IO Word32
data ErrFriendCustomPacket
    = ErrFriendCustomPacketNull
    | ErrFriendCustomPacketFriendNotFound
    | ErrFriendCustomPacketFriendNotConnected
    | ErrFriendCustomPacketInvalid
    | ErrFriendCustomPacketEmpty
    | ErrFriendCustomPacketTooLong
    | ErrFriendCustomPacketSendq
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrFriendCustomPacket
instance Arbitrary ErrFriendCustomPacket where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_friend_send_lossy_packet :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
foreign import ccall tox_friend_send_lossless_packet :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
type FriendLossyPacketCb = ToxPtr -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendLossyPacketCb :: FriendLossyPacketCb -> IO (FunPtr FriendLossyPacketCb)
foreign import ccall tox_callback_friend_lossy_packet :: ToxPtr -> FunPtr FriendLossyPacketCb -> IO ()
type FriendLosslessPacketCb = ToxPtr -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapFriendLosslessPacketCb :: FriendLosslessPacketCb -> IO (FunPtr FriendLosslessPacketCb)
foreign import ccall tox_callback_friend_lossless_packet :: ToxPtr -> FunPtr FriendLosslessPacketCb -> IO ()
data ErrGetPort
    = ErrGetPortNotBound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGetPort
instance Arbitrary ErrGetPort where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_self_get_dht_id :: ToxPtr -> CString -> IO ()
foreign import ccall tox_self_get_udp_port :: ToxPtr -> CErr ErrGetPort -> IO Word16
foreign import ccall tox_self_get_tcp_port :: ToxPtr -> CErr ErrGetPort -> IO Word16
foreign import ccall tox_group_max_topic_length :: Word32
foreign import ccall tox_group_max_part_length :: Word32
foreign import ccall tox_group_max_message_length :: Word32
foreign import ccall tox_group_max_custom_lossy_packet_length :: Word32
foreign import ccall tox_group_max_custom_lossless_packet_length :: Word32
foreign import ccall tox_group_max_group_name_length :: Word32
foreign import ccall tox_group_max_password_size :: Word32
foreign import ccall tox_group_chat_id_size :: Word32
foreign import ccall tox_group_peer_public_key_size :: Word32
data GroupPrivacyState
    = GroupPrivacyStatePublic
    | GroupPrivacyStatePrivate
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupPrivacyState
instance Arbitrary GroupPrivacyState where arbitrary = arbitraryBoundedEnum
data GroupTopicLock
    = GroupTopicLockEnabled
    | GroupTopicLockDisabled
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupTopicLock
instance Arbitrary GroupTopicLock where arbitrary = arbitraryBoundedEnum
data GroupVoiceState
    = GroupVoiceStateAll
    | GroupVoiceStateModerator
    | GroupVoiceStateFounder
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupVoiceState
instance Arbitrary GroupVoiceState where arbitrary = arbitraryBoundedEnum
data GroupRole
    = GroupRoleFounder
    | GroupRoleModerator
    | GroupRoleUser
    | GroupRoleObserver
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupRole
instance Arbitrary GroupRole where arbitrary = arbitraryBoundedEnum
data ErrGroupNew
    = ErrGroupNewTooLong
    | ErrGroupNewEmpty
    | ErrGroupNewInit
    | ErrGroupNewState
    | ErrGroupNewAnnounce
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupNew
instance Arbitrary ErrGroupNew where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_new :: ToxPtr -> CEnum GroupPrivacyState -> CString -> CSize -> CString -> CSize -> CErr ErrGroupNew -> IO Word32
data ErrGroupJoin
    = ErrGroupJoinInit
    | ErrGroupJoinBadChatId
    | ErrGroupJoinEmpty
    | ErrGroupJoinTooLong
    | ErrGroupJoinPassword
    | ErrGroupJoinCore
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupJoin
instance Arbitrary ErrGroupJoin where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_join :: ToxPtr -> CString -> CString -> CSize -> CString -> CSize -> CErr ErrGroupJoin -> IO Word32
data ErrGroupIsConnected
    = ErrGroupIsConnectedGroupNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupIsConnected
instance Arbitrary ErrGroupIsConnected where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_is_connected :: ToxPtr -> Word32 -> CErr ErrGroupIsConnected -> IO Bool
data ErrGroupDisconnect
    = ErrGroupDisconnectGroupNotFound
    | ErrGroupDisconnectAlreadyDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupDisconnect
instance Arbitrary ErrGroupDisconnect where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_disconnect :: ToxPtr -> Word32 -> CErr ErrGroupDisconnect -> IO Bool
data ErrGroupReconnect
    = ErrGroupReconnectGroupNotFound
    | ErrGroupReconnectCore
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupReconnect
instance Arbitrary ErrGroupReconnect where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_reconnect :: ToxPtr -> Word32 -> CErr ErrGroupReconnect -> IO Bool
data ErrGroupLeave
    = ErrGroupLeaveGroupNotFound
    | ErrGroupLeaveTooLong
    | ErrGroupLeaveFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupLeave
instance Arbitrary ErrGroupLeave where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_leave :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrGroupLeave -> IO Bool
data ErrGroupSelfQuery
    = ErrGroupSelfQueryGroupNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSelfQuery
instance Arbitrary ErrGroupSelfQuery where arbitrary = arbitraryBoundedEnum
data ErrGroupSelfNameSet
    = ErrGroupSelfNameSetGroupNotFound
    | ErrGroupSelfNameSetTooLong
    | ErrGroupSelfNameSetInvalid
    | ErrGroupSelfNameSetFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSelfNameSet
instance Arbitrary ErrGroupSelfNameSet where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_self_set_name :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrGroupSelfNameSet -> IO Bool
foreign import ccall tox_group_self_get_name_size :: ToxPtr -> Word32 -> CErr ErrGroupSelfQuery -> IO CSize
foreign import ccall tox_group_self_get_name :: ToxPtr -> Word32 -> CString -> CErr ErrGroupSelfQuery -> IO Bool
data ErrGroupSelfStatusSet
    = ErrGroupSelfStatusSetGroupNotFound
    | ErrGroupSelfStatusSetFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSelfStatusSet
instance Arbitrary ErrGroupSelfStatusSet where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_self_set_status :: ToxPtr -> Word32 -> CEnum UserStatus -> CErr ErrGroupSelfStatusSet -> IO Bool
foreign import ccall tox_group_self_get_status :: ToxPtr -> Word32 -> CErr ErrGroupSelfQuery -> IO (CEnum UserStatus)
foreign import ccall tox_group_self_get_role :: ToxPtr -> Word32 -> CErr ErrGroupSelfQuery -> IO (CEnum GroupRole)
foreign import ccall tox_group_self_get_peer_id :: ToxPtr -> Word32 -> CErr ErrGroupSelfQuery -> IO Word32
foreign import ccall tox_group_self_get_public_key :: ToxPtr -> Word32 -> CString -> CErr ErrGroupSelfQuery -> IO Bool
data ErrGroupPeerQuery
    = ErrGroupPeerQueryGroupNotFound
    | ErrGroupPeerQueryPeerNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupPeerQuery
instance Arbitrary ErrGroupPeerQuery where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_peer_get_name_size :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupPeerQuery -> IO CSize
foreign import ccall tox_group_peer_get_name :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrGroupPeerQuery -> IO Bool
foreign import ccall tox_group_peer_get_status :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupPeerQuery -> IO (CEnum UserStatus)
foreign import ccall tox_group_peer_get_role :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupPeerQuery -> IO (CEnum GroupRole)
foreign import ccall tox_group_peer_get_connection_status :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupPeerQuery -> IO (CEnum Connection)
foreign import ccall tox_group_peer_get_public_key :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrGroupPeerQuery -> IO Bool
type GroupPeerNameCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerNameCb :: GroupPeerNameCb -> IO (FunPtr GroupPeerNameCb)
foreign import ccall tox_callback_group_peer_name :: ToxPtr -> FunPtr GroupPeerNameCb -> IO ()
type GroupPeerStatusCb = ToxPtr -> Word32 -> Word32 -> CEnum UserStatus -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerStatusCb :: GroupPeerStatusCb -> IO (FunPtr GroupPeerStatusCb)
foreign import ccall tox_callback_group_peer_status :: ToxPtr -> FunPtr GroupPeerStatusCb -> IO ()
data ErrGroupStateQueries
    = ErrGroupStateQueriesGroupNotFound
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupStateQueries
instance Arbitrary ErrGroupStateQueries where arbitrary = arbitraryBoundedEnum
data ErrGroupTopicSet
    = ErrGroupTopicSetGroupNotFound
    | ErrGroupTopicSetTooLong
    | ErrGroupTopicSetPermissions
    | ErrGroupTopicSetFailCreate
    | ErrGroupTopicSetFailSend
    | ErrGroupTopicSetDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupTopicSet
instance Arbitrary ErrGroupTopicSet where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_set_topic :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrGroupTopicSet -> IO Bool
foreign import ccall tox_group_get_topic_size :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO CSize
foreign import ccall tox_group_get_topic :: ToxPtr -> Word32 -> CString -> CErr ErrGroupStateQueries -> IO Bool
type GroupTopicCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupTopicCb :: GroupTopicCb -> IO (FunPtr GroupTopicCb)
foreign import ccall tox_callback_group_topic :: ToxPtr -> FunPtr GroupTopicCb -> IO ()
foreign import ccall tox_group_get_name_size :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO CSize
foreign import ccall tox_group_get_name :: ToxPtr -> Word32 -> CString -> CErr ErrGroupStateQueries -> IO Bool
foreign import ccall tox_group_get_chat_id :: ToxPtr -> Word32 -> CString -> CErr ErrGroupStateQueries -> IO Bool
foreign import ccall tox_group_get_number_groups :: ToxPtr -> IO Word32
foreign import ccall tox_group_get_privacy_state :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO (CEnum GroupPrivacyState)
type GroupPrivacyStateCb = ToxPtr -> Word32 -> CEnum GroupPrivacyState -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPrivacyStateCb :: GroupPrivacyStateCb -> IO (FunPtr GroupPrivacyStateCb)
foreign import ccall tox_callback_group_privacy_state :: ToxPtr -> FunPtr GroupPrivacyStateCb -> IO ()
foreign import ccall tox_group_get_voice_state :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO (CEnum GroupVoiceState)
type GroupVoiceStateCb = ToxPtr -> Word32 -> CEnum GroupVoiceState -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupVoiceStateCb :: GroupVoiceStateCb -> IO (FunPtr GroupVoiceStateCb)
foreign import ccall tox_callback_group_voice_state :: ToxPtr -> FunPtr GroupVoiceStateCb -> IO ()
foreign import ccall tox_group_get_topic_lock :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO (CEnum GroupTopicLock)
type GroupTopicLockCb = ToxPtr -> Word32 -> CEnum GroupTopicLock -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupTopicLockCb :: GroupTopicLockCb -> IO (FunPtr GroupTopicLockCb)
foreign import ccall tox_callback_group_topic_lock :: ToxPtr -> FunPtr GroupTopicLockCb -> IO ()
foreign import ccall tox_group_get_peer_limit :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO Word16
type GroupPeerLimitCb = ToxPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerLimitCb :: GroupPeerLimitCb -> IO (FunPtr GroupPeerLimitCb)
foreign import ccall tox_callback_group_peer_limit :: ToxPtr -> FunPtr GroupPeerLimitCb -> IO ()
foreign import ccall tox_group_get_password_size :: ToxPtr -> Word32 -> CErr ErrGroupStateQueries -> IO CSize
foreign import ccall tox_group_get_password :: ToxPtr -> Word32 -> CString -> CErr ErrGroupStateQueries -> IO Bool
type GroupPasswordCb = ToxPtr -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPasswordCb :: GroupPasswordCb -> IO (FunPtr GroupPasswordCb)
foreign import ccall tox_callback_group_password :: ToxPtr -> FunPtr GroupPasswordCb -> IO ()
data ErrGroupSendMessage
    = ErrGroupSendMessageGroupNotFound
    | ErrGroupSendMessageTooLong
    | ErrGroupSendMessageEmpty
    | ErrGroupSendMessageBadType
    | ErrGroupSendMessagePermissions
    | ErrGroupSendMessageFailSend
    | ErrGroupSendMessageDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSendMessage
instance Arbitrary ErrGroupSendMessage where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_send_message :: ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> Ptr Word32 -> CErr ErrGroupSendMessage -> IO Bool
data ErrGroupSendPrivateMessage
    = ErrGroupSendPrivateMessageGroupNotFound
    | ErrGroupSendPrivateMessagePeerNotFound
    | ErrGroupSendPrivateMessageTooLong
    | ErrGroupSendPrivateMessageEmpty
    | ErrGroupSendPrivateMessagePermissions
    | ErrGroupSendPrivateMessageFailSend
    | ErrGroupSendPrivateMessageDisconnected
    | ErrGroupSendPrivateMessageBadType
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSendPrivateMessage
instance Arbitrary ErrGroupSendPrivateMessage where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_send_private_message :: ToxPtr -> Word32 -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrGroupSendPrivateMessage -> IO Bool
data ErrGroupSendCustomPacket
    = ErrGroupSendCustomPacketGroupNotFound
    | ErrGroupSendCustomPacketTooLong
    | ErrGroupSendCustomPacketEmpty
    | ErrGroupSendCustomPacketPermissions
    | ErrGroupSendCustomPacketDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSendCustomPacket
instance Arbitrary ErrGroupSendCustomPacket where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_send_custom_packet :: ToxPtr -> Word32 -> Bool -> CString -> CSize -> CErr ErrGroupSendCustomPacket -> IO Bool
data ErrGroupSendCustomPrivatePacket
    = ErrGroupSendCustomPrivatePacketGroupNotFound
    | ErrGroupSendCustomPrivatePacketTooLong
    | ErrGroupSendCustomPrivatePacketEmpty
    | ErrGroupSendCustomPrivatePacketPeerNotFound
    | ErrGroupSendCustomPrivatePacketPermissions
    | ErrGroupSendCustomPrivatePacketFailSend
    | ErrGroupSendCustomPrivatePacketDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSendCustomPrivatePacket
instance Arbitrary ErrGroupSendCustomPrivatePacket where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_send_custom_private_packet :: ToxPtr -> Word32 -> Word32 -> Bool -> CString -> CSize -> CErr ErrGroupSendCustomPrivatePacket -> IO Bool
type GroupMessageCb = ToxPtr -> Word32 -> Word32 -> CEnum MessageType -> CString -> CSize -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupMessageCb :: GroupMessageCb -> IO (FunPtr GroupMessageCb)
foreign import ccall tox_callback_group_message :: ToxPtr -> FunPtr GroupMessageCb -> IO ()
type GroupPrivateMessageCb = ToxPtr -> Word32 -> Word32 -> CEnum MessageType -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPrivateMessageCb :: GroupPrivateMessageCb -> IO (FunPtr GroupPrivateMessageCb)
foreign import ccall tox_callback_group_private_message :: ToxPtr -> FunPtr GroupPrivateMessageCb -> IO ()
type GroupCustomPacketCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupCustomPacketCb :: GroupCustomPacketCb -> IO (FunPtr GroupCustomPacketCb)
foreign import ccall tox_callback_group_custom_packet :: ToxPtr -> FunPtr GroupCustomPacketCb -> IO ()
type GroupCustomPrivatePacketCb = ToxPtr -> Word32 -> Word32 -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupCustomPrivatePacketCb :: GroupCustomPrivatePacketCb -> IO (FunPtr GroupCustomPrivatePacketCb)
foreign import ccall tox_callback_group_custom_private_packet :: ToxPtr -> FunPtr GroupCustomPrivatePacketCb -> IO ()
data ErrGroupInviteFriend
    = ErrGroupInviteFriendGroupNotFound
    | ErrGroupInviteFriendFriendNotFound
    | ErrGroupInviteFriendInviteFail
    | ErrGroupInviteFriendFailSend
    | ErrGroupInviteFriendDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupInviteFriend
instance Arbitrary ErrGroupInviteFriend where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_invite_friend :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupInviteFriend -> IO Bool
data ErrGroupInviteAccept
    = ErrGroupInviteAcceptBadInvite
    | ErrGroupInviteAcceptInitFailed
    | ErrGroupInviteAcceptTooLong
    | ErrGroupInviteAcceptEmpty
    | ErrGroupInviteAcceptPassword
    | ErrGroupInviteAcceptCore
    | ErrGroupInviteAcceptFailSend
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupInviteAccept
instance Arbitrary ErrGroupInviteAccept where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_invite_accept :: ToxPtr -> Word32 -> CString -> CSize -> CString -> CSize -> CString -> CSize -> CErr ErrGroupInviteAccept -> IO Word32
type GroupInviteCb = ToxPtr -> Word32 -> CString -> CSize -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupInviteCb :: GroupInviteCb -> IO (FunPtr GroupInviteCb)
foreign import ccall tox_callback_group_invite :: ToxPtr -> FunPtr GroupInviteCb -> IO ()
type GroupPeerJoinCb = ToxPtr -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerJoinCb :: GroupPeerJoinCb -> IO (FunPtr GroupPeerJoinCb)
foreign import ccall tox_callback_group_peer_join :: ToxPtr -> FunPtr GroupPeerJoinCb -> IO ()
data GroupExitType
    = GroupExitTypeQuit
    | GroupExitTypeTimeout
    | GroupExitTypeDisconnected
    | GroupExitTypeSelfDisconnected
    | GroupExitTypeKick
    | GroupExitTypeSyncError
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupExitType
instance Arbitrary GroupExitType where arbitrary = arbitraryBoundedEnum
type GroupPeerExitCb = ToxPtr -> Word32 -> Word32 -> CEnum GroupExitType -> CString -> CSize -> CString -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupPeerExitCb :: GroupPeerExitCb -> IO (FunPtr GroupPeerExitCb)
foreign import ccall tox_callback_group_peer_exit :: ToxPtr -> FunPtr GroupPeerExitCb -> IO ()
type GroupSelfJoinCb = ToxPtr -> Word32 -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupSelfJoinCb :: GroupSelfJoinCb -> IO (FunPtr GroupSelfJoinCb)
foreign import ccall tox_callback_group_self_join :: ToxPtr -> FunPtr GroupSelfJoinCb -> IO ()
data GroupJoinFail
    = GroupJoinFailPeerLimit
    | GroupJoinFailInvalidPassword
    | GroupJoinFailUnknown
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupJoinFail
instance Arbitrary GroupJoinFail where arbitrary = arbitraryBoundedEnum
type GroupJoinFailCb = ToxPtr -> Word32 -> CEnum GroupJoinFail -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupJoinFailCb :: GroupJoinFailCb -> IO (FunPtr GroupJoinFailCb)
foreign import ccall tox_callback_group_join_fail :: ToxPtr -> FunPtr GroupJoinFailCb -> IO ()
data ErrGroupFounderSetPassword
    = ErrGroupFounderSetPasswordGroupNotFound
    | ErrGroupFounderSetPasswordPermissions
    | ErrGroupFounderSetPasswordTooLong
    | ErrGroupFounderSetPasswordFailSend
    | ErrGroupFounderSetPasswordMalloc
    | ErrGroupFounderSetPasswordDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupFounderSetPassword
instance Arbitrary ErrGroupFounderSetPassword where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_founder_set_password :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrGroupFounderSetPassword -> IO Bool
data ErrGroupFounderSetTopicLock
    = ErrGroupFounderSetTopicLockGroupNotFound
    | ErrGroupFounderSetTopicLockInvalid
    | ErrGroupFounderSetTopicLockPermissions
    | ErrGroupFounderSetTopicLockFailSet
    | ErrGroupFounderSetTopicLockFailSend
    | ErrGroupFounderSetTopicLockDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupFounderSetTopicLock
instance Arbitrary ErrGroupFounderSetTopicLock where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_founder_set_topic_lock :: ToxPtr -> Word32 -> CEnum GroupTopicLock -> CErr ErrGroupFounderSetTopicLock -> IO Bool
data ErrGroupFounderSetVoiceState
    = ErrGroupFounderSetVoiceStateGroupNotFound
    | ErrGroupFounderSetVoiceStatePermissions
    | ErrGroupFounderSetVoiceStateFailSet
    | ErrGroupFounderSetVoiceStateFailSend
    | ErrGroupFounderSetVoiceStateDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupFounderSetVoiceState
instance Arbitrary ErrGroupFounderSetVoiceState where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_founder_set_voice_state :: ToxPtr -> Word32 -> CEnum GroupVoiceState -> CErr ErrGroupFounderSetVoiceState -> IO Bool
data ErrGroupFounderSetPrivacyState
    = ErrGroupFounderSetPrivacyStateGroupNotFound
    | ErrGroupFounderSetPrivacyStatePermissions
    | ErrGroupFounderSetPrivacyStateFailSet
    | ErrGroupFounderSetPrivacyStateFailSend
    | ErrGroupFounderSetPrivacyStateDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupFounderSetPrivacyState
instance Arbitrary ErrGroupFounderSetPrivacyState where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_founder_set_privacy_state :: ToxPtr -> Word32 -> CEnum GroupPrivacyState -> CErr ErrGroupFounderSetPrivacyState -> IO Bool
data ErrGroupFounderSetPeerLimit
    = ErrGroupFounderSetPeerLimitGroupNotFound
    | ErrGroupFounderSetPeerLimitPermissions
    | ErrGroupFounderSetPeerLimitFailSet
    | ErrGroupFounderSetPeerLimitFailSend
    | ErrGroupFounderSetPeerLimitDisconnected
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupFounderSetPeerLimit
instance Arbitrary ErrGroupFounderSetPeerLimit where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_founder_set_peer_limit :: ToxPtr -> Word32 -> Word16 -> CErr ErrGroupFounderSetPeerLimit -> IO Bool
data ErrGroupSetIgnore
    = ErrGroupSetIgnoreGroupNotFound
    | ErrGroupSetIgnorePeerNotFound
    | ErrGroupSetIgnoreSelf
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupSetIgnore
instance Arbitrary ErrGroupSetIgnore where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_set_ignore :: ToxPtr -> Word32 -> Word32 -> Bool -> CErr ErrGroupSetIgnore -> IO Bool
data ErrGroupModSetRole
    = ErrGroupModSetRoleGroupNotFound
    | ErrGroupModSetRolePeerNotFound
    | ErrGroupModSetRolePermissions
    | ErrGroupModSetRoleAssignment
    | ErrGroupModSetRoleFailAction
    | ErrGroupModSetRoleSelf
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupModSetRole
instance Arbitrary ErrGroupModSetRole where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_mod_set_role :: ToxPtr -> Word32 -> Word32 -> CEnum GroupRole -> CErr ErrGroupModSetRole -> IO Bool
data ErrGroupModKickPeer
    = ErrGroupModKickPeerGroupNotFound
    | ErrGroupModKickPeerPeerNotFound
    | ErrGroupModKickPeerPermissions
    | ErrGroupModKickPeerFailAction
    | ErrGroupModKickPeerFailSend
    | ErrGroupModKickPeerSelf
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack ErrGroupModKickPeer
instance Arbitrary ErrGroupModKickPeer where arbitrary = arbitraryBoundedEnum
foreign import ccall tox_group_mod_kick_peer :: ToxPtr -> Word32 -> Word32 -> CErr ErrGroupModKickPeer -> IO Bool
data GroupModEvent
    = GroupModEventKick
    | GroupModEventObserver
    | GroupModEventUser
    | GroupModEventModerator
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)
instance MessagePack GroupModEvent
instance Arbitrary GroupModEvent where arbitrary = arbitraryBoundedEnum
type GroupModerationCb = ToxPtr -> Word32 -> Word32 -> Word32 -> CEnum GroupModEvent -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapGroupModerationCb :: GroupModerationCb -> IO (FunPtr GroupModerationCb)
foreign import ccall tox_callback_group_moderation :: ToxPtr -> FunPtr GroupModerationCb -> IO ()
