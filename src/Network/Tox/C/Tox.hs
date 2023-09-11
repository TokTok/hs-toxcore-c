{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | Public core API for Tox clients.
--
-- Every function that can fail takes a function-specific error code pointer
-- that can be used to diagnose problems with the Tox state or the function
-- arguments. The error code pointer can be 'nullPtr', which does not influence
-- the function's behaviour, but can be done if the reason for failure is
-- irrelevant to the client.
--
-- The exception to this rule are simple allocation functions whose only failure
-- mode is allocation failure. They return 'nullPtr' in that case, and do not
-- set an error code.
--
-- Every error code type has an OK value to which functions will set their error
-- code value on success. Clients can keep their error code uninitialised before
-- passing it to a function. The library guarantees that after returning, the
-- value pointed to by the error code pointer has been initialised.
--
-- Functions with pointer parameters often have a 'nullPtr' error code, meaning
-- they could not perform any operation, because one of the required parameters
-- was 'nullPtr'. Some functions operate correctly or are defined as effectless
-- on 'nullPtr'.
--
-- Some functions additionally return a value outside their return type domain,
-- or a bool containing true on success and false on failure.
--
-- All functions that take a Tox instance pointer will cause undefined behaviour
-- when passed a 'nullPtr' Tox pointer.
--
-- All integer values are expected in host byte order.
--
-- Functions with parameters with enum types cause unspecified behaviour if the
-- enumeration value is outside the valid range of the type. If possible, the
-- function will try to use a sane default, but there will be no error code, and
-- one possible action for the function to take is to have no effect.
--
-- \subsection events Events and callbacks
--
-- Events are handled by callbacks. One callback can be registered per event.
-- All events have a callback function type named `tox_{event}_cb` and a
-- function to register it named `tox_callback_{event}`. Passing a 'nullPtr'
-- callback will result in no callback being registered for that event. Only one
-- callback per event can be registered, so if a client needs multiple event
-- listeners, it needs to implement the dispatch functionality itself.
--
-- \subsection threading Threading implications
--
-- It is possible to run multiple concurrent threads with a Tox instance for
-- each thread. It is also possible to run all Tox instances in the same thread.
-- A common way to run Tox (multiple or single instance) is to have one thread
-- running a simple tox_iterate loop, sleeping for tox_iteration_interval
-- milliseconds on each iteration.
--
-- If you want to access a single Tox instance from multiple threads, access to
-- the instance must be synchronised. While multiple threads can concurrently
-- access multiple different Tox instances, no more than one API function can
-- operate on a single instance at any given time.
--
-- Functions that write to variable length byte arrays will always have a size
-- function associated with them. The result of this size function is only valid
-- until another mutating function (one that takes a pointer to non-const Tox)
-- is called. Thus, clients must ensure that no other thread calls a mutating
-- function between the call to the size function and the call to the retrieval
-- function.
--
-- E.g. to get the current nickname, one would write
--
-- \code
-- CSize length = tox_self_get_name_size(tox);
-- CString name = malloc(length);
-- if (!name) abort();
-- tox_self_get_name(tox, name);
-- \endcode
--
-- If any other thread calls tox_self_set_name while this thread is allocating
-- memory, the length may have become invalid, and the call to tox_self_get_name
-- may cause undefined behaviour.
--
module Network.Tox.C.Tox where

import           Control.Exception        (bracket)
import           Control.Monad            ((>=>))
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.MessagePack         as MP
import           Data.Word                (Word16, Word32, Word64)
import           Foreign.C.Enum
import           Foreign.C.String         (CString, withCString)
import           Foreign.C.Types          (CTime (..))
import           Foreign.Marshal.Array    (allocaArray, peekArray)
import           Foreign.Ptr              (Ptr, nullPtr)
import           System.Posix.Types       (EpochTime)

import           FFI.Tox.Tox              (ConferenceType, Connection,
                                           ErrBootstrap (..),
                                           ErrConferenceDelete (..),
                                           ErrConferenceGetType (..),
                                           ErrConferenceInvite (..),
                                           ErrConferenceJoin (..),
                                           ErrConferenceNew (..),
                                           ErrConferencePeerQuery (..),
                                           ErrConferenceSendMessage (..),
                                           ErrConferenceTitle (..),
                                           ErrFileControl (..), ErrFileGet (..),
                                           ErrFileSeek (..), ErrFileSend (..),
                                           ErrFileSendChunk (..),
                                           ErrFriendAdd (..),
                                           ErrFriendByPublicKey (..),
                                           ErrFriendCustomPacket (..),
                                           ErrFriendDelete (..),
                                           ErrFriendGetLastOnline (..),
                                           ErrFriendGetPublicKey (..),
                                           ErrFriendQuery (..),
                                           ErrFriendSendMessage (..),
                                           ErrGetPort (..), ErrNew (..),
                                           ErrSetInfo (..), ErrSetTyping (..),
                                           FileControl, FileKind (..),
                                           MessageType, ToxPtr, UserStatus,
                                           tox_add_tcp_relay, tox_bootstrap,
                                           tox_conference_delete,
                                           tox_conference_get_chatlist,
                                           tox_conference_get_chatlist_size,
                                           tox_conference_get_title,
                                           tox_conference_get_title_size,
                                           tox_conference_get_type,
                                           tox_conference_invite,
                                           tox_conference_join,
                                           tox_conference_new,
                                           tox_conference_peer_count,
                                           tox_conference_peer_get_name,
                                           tox_conference_peer_get_name_size,
                                           tox_conference_peer_get_public_key,
                                           tox_conference_peer_number_is_ours,
                                           tox_conference_send_message,
                                           tox_conference_set_title,
                                           tox_file_control,
                                           tox_file_get_file_id, tox_file_seek,
                                           tox_file_send, tox_file_send_chunk,
                                           tox_friend_add,
                                           tox_friend_add_norequest,
                                           tox_friend_by_public_key,
                                           tox_friend_delete, tox_friend_exists,
                                           tox_friend_get_connection_status,
                                           tox_friend_get_last_online,
                                           tox_friend_get_name,
                                           tox_friend_get_name_size,
                                           tox_friend_get_public_key,
                                           tox_friend_get_status_message,
                                           tox_friend_get_status_message_size,
                                           tox_friend_get_typing,
                                           tox_friend_send_lossless_packet,
                                           tox_friend_send_lossy_packet,
                                           tox_friend_send_message,
                                           tox_get_savedata,
                                           tox_get_savedata_size, tox_hash,
                                           tox_iteration_interval, tox_kill,
                                           tox_new, tox_self_get_address,
                                           tox_self_get_dht_id,
                                           tox_self_get_friend_list,
                                           tox_self_get_friend_list_size,
                                           tox_self_get_name,
                                           tox_self_get_name_size,
                                           tox_self_get_nospam,
                                           tox_self_get_public_key,
                                           tox_self_get_secret_key,
                                           tox_self_get_status_message,
                                           tox_self_get_status_message_size,
                                           tox_self_get_tcp_port,
                                           tox_self_get_udp_port,
                                           tox_self_set_name,
                                           tox_self_set_nospam,
                                           tox_self_set_status,
                                           tox_self_set_status_message,
                                           tox_self_set_typing)
import           Network.Tox.C.Constants
import           Network.Tox.C.Options
import           Network.Tox.Types.Events (Event)


--------------------------------------------------------------------------------
--
-- :: Global types
--
--------------------------------------------------------------------------------

-- Should we introduce such types?
-- newtype FriendNum = FriendNum { friendNum :: Word32 } deriving (Eq, Ord, Read, Show)
-- newtype ConferenceNum = ConferenceNum { conferenceNum :: Word32 } deriving (Eq, Ord, Read, Show)
-- newtype PeerNum = PeerNum { peerNum :: Word32 } deriving (Eq, Ord, Read, Show)
-- newtype FileNum = FileNum { fileNum :: Word32 } deriving (Eq, Ord, Read, Show)


--------------------------------------------------------------------------------
--
-- :: Creation and destruction
--
--------------------------------------------------------------------------------

withTox :: Options -> (ToxPtr -> IO a) -> IO (Either ErrNew a)
withTox opts f =
    fmap mapErr . withOptions opts $ \copts -> do
        result <- callErrFun . tox_new $ copts
        case result of
            Left err ->
                return $ Left err
            Right tox -> do
                tox_events_init tox
                res <- f tox
                tox_kill tox
                return $ Right res

  where
    mapErr (Left ErrOptionsNewMalloc) = Left ErrNewMalloc
    mapErr (Right ok)                 = ok

toxGetSavedata :: ToxPtr -> IO BS.ByteString
toxGetSavedata tox = do
    savedataLen <- tox_get_savedata_size tox
    allocaArray (fromIntegral savedataLen) $ \savedataPtr -> do
        tox_get_savedata tox savedataPtr
        BS.packCStringLen (savedataPtr, fromIntegral savedataLen)


--------------------------------------------------------------------------------
--
-- :: Connection lifecycle and event loop
--
--------------------------------------------------------------------------------


-- | Sends a "get nodes" request to the given bootstrap node with IP, port, and
-- public key to setup connections.
--
-- This function will attempt to connect to the node using UDP. You must use
-- this function even if Options.udp_enabled was set to false.
--
-- @param address The hostname or IP address (IPv4 or IPv6) of the node.
-- @param port The port on the host on which the bootstrap Tox instance is
--   listening.
-- @param public_key The long term public key of the bootstrap node
--   ('tox_public_key_size' bytes).
-- @return true on success.
toxBootstrap :: ToxPtr -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap Bool)
toxBootstrap tox address port pubKey =
    withCString address $ \address' ->
        BS.useAsCString pubKey $ \pubKey' ->
            callErrFun $ tox_bootstrap tox address' (fromIntegral port) pubKey'


-- | Adds additional host:port pair as TCP relay.
--
-- This function can be used to initiate TCP connections to different ports on
-- the same bootstrap node, or to add TCP relays without using them as
-- bootstrap nodes.
--
-- @param address The hostname or IP address (IPv4 or IPv6) of the TCP relay.
-- @param port The port on the host on which the TCP relay is listening.
-- @param public_key The long term public key of the TCP relay
--   ('tox_public_key_size' bytes).
-- @return true on success.
toxAddTcpRelay :: ToxPtr -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap Bool)
toxAddTcpRelay tox address port pubKey =
    withCString address $ \address' ->
        BS.useAsCString pubKey $ \pubKey' ->
            callErrFun $ tox_add_tcp_relay tox address' (fromIntegral port) pubKey'


data ToxEventsStruct
type ToxEvents = Ptr ToxEventsStruct

-- | Common error codes for all functions that set a piece of user-visible
-- client information.
data ErrEventsIterate
    = ErrEventsIterateMalloc
      -- The function failed to allocate enough memory to store the events.

    | ErrEventsDecode
      -- Failed to encode or decode events in msgpack.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

foreign  import ccall tox_events_init :: ToxPtr -> IO ()
foreign  import ccall tox_events_iterate :: ToxPtr -> Bool -> CErr ErrEventsIterate -> IO ToxEvents

foreign  import ccall tox_events_bytes_size :: ToxEvents -> IO Word32
foreign  import ccall tox_events_get_bytes :: ToxEvents -> CString -> IO ()

foreign import ccall tox_events_load :: ToxSystem -> CString -> Word32 -> IO ToxEvents
foreign import ccall tox_events_free :: ToxEvents -> IO ()

toxEventsToPtr :: [Event] -> IO ToxEvents
toxEventsToPtr events = do
    let encoded = MP.pack events
    sys <- os_system
    BS.useAsCStringLen (LBS.toStrict encoded) $ \(ptr, len) ->
        tox_events_load sys ptr (fromIntegral len)

toxEventsFromPtr :: ToxEvents -> IO (Either String [Event])
toxEventsFromPtr evPtr = do
    bytes <- bracket (return evPtr) tox_events_free $ const $ do
        len <- tox_events_bytes_size evPtr
        allocaArray (fromIntegral len) $ \ptr -> do
            tox_events_get_bytes evPtr ptr
            BS.packCStringLen (ptr, fromIntegral len)
    case MP.unpackEither $ LBS.fromStrict bytes of
        Left err -> do
            print (MP.unpackEither $ LBS.fromStrict bytes :: Either MP.DecodeError MP.Object)
            return $ Left $ show err
        Right ok -> return $ Right ok

toxEventsIterate :: ToxPtr -> IO (Either String [Event])
toxEventsIterate tox =
    callErrFun (tox_events_iterate tox True) >>= \case
        Left err    -> return $ Left $ show err
        Right evPtr -> toxEventsFromPtr evPtr


-- | Return the time in milliseconds before tox_iterate() should be called again
-- for optimal performance.
toxIterationInterval :: ToxPtr -> IO Word32
toxIterationInterval = tox_iteration_interval

--------------------------------------------------------------------------------
--
-- :: Internal client information (Tox address/id)
--
--------------------------------------------------------------------------------


-- | Writes the Tox friend address of the client to a byte array. The address is
-- not in human-readable format. If a client wants to display the address,
-- formatting is required.
--
-- @param address A memory region of at least 'tox_address_size' bytes. If this
--   parameter is 'nullPtr', this function has no effect.
-- @see 'tox_address_size' for the address format.
toxSelfGetAddress :: ToxPtr -> IO BS.ByteString
toxSelfGetAddress tox =
    let addrLen = fromIntegral tox_address_size in
    allocaArray addrLen $ \addrPtr -> do
        tox_self_get_address tox addrPtr
        BS.packCStringLen (addrPtr, addrLen)

-- | Set the 4-byte nospam part of the address.
--
-- @param nospam Any 32 bit unsigned integer.
toxSelfSetNospam :: ToxPtr -> Word32 -> IO ()
toxSelfSetNospam = tox_self_set_nospam

-- | Get the 4-byte nospam part of the address.
toxSelfGetNospam :: ToxPtr -> IO Word32
toxSelfGetNospam = tox_self_get_nospam

-- | Copy the Tox Public Key (long term) from the Tox object.
--
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
toxSelfGetPublicKey :: ToxPtr -> IO BS.ByteString
toxSelfGetPublicKey tox =
    let pkLen = fromIntegral tox_public_key_size in
    allocaArray pkLen $ \pkPtr -> do
        tox_self_get_public_key tox pkPtr
        BS.packCStringLen (pkPtr, pkLen)

-- | Copy the Tox Secret Key from the Tox object.
--
-- @param secret_key A memory region of at least 'tox_secret_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
toxSelfGetSecretKey :: ToxPtr -> IO BS.ByteString
toxSelfGetSecretKey tox =
    let skLen = fromIntegral tox_secret_key_size in
    allocaArray skLen $ \skPtr -> do
        tox_self_get_secret_key tox skPtr
        BS.packCStringLen (skPtr, skLen)


--------------------------------------------------------------------------------
--
-- :: User-visible client information (nickname/status)
--
--------------------------------------------------------------------------------


-- | Set the nickname for the Tox client.
--
-- Nickname length cannot exceed 'tox_max_name_length'. If length is 0, the name
-- parameter is ignored (it can be 'nullPtr'), and the nickname is set back to
-- empty.
--
-- @param name A byte array containing the new nickname.
-- @param length The size of the name byte array.
--
-- @return true on success.
toxSelfSetName :: ToxPtr -> BS.ByteString -> IO (Either ErrSetInfo Bool)
toxSelfSetName tox name =
    BS.useAsCStringLen name $ \(nameStr, nameLen) ->
        callErrFun $ tox_self_set_name tox nameStr (fromIntegral nameLen)

-- | Write the nickname set by tox_self_set_name to a byte array.
--
-- If no nickname was set before calling this function, the name is empty,
-- and this function has no effect.
--
-- Call tox_self_get_name_size to find out how much memory to allocate for
-- the result.
--
-- @param name A valid memory location large enough to hold the nickname.
--   If this parameter is NULL, the function has no effect.
toxSelfGetName :: ToxPtr -> IO BS.ByteString
toxSelfGetName tox = do
    nameLen <- tox_self_get_name_size tox
    allocaArray (fromIntegral nameLen) $ \namePtr -> do
        tox_self_get_name tox namePtr
        BS.packCStringLen (namePtr, fromIntegral nameLen)


-- | Set the client's status message.
--
-- Status message length cannot exceed 'tox_max_status_message_length'. If
-- length is 0, the status parameter is ignored (it can be 'nullPtr'), and the
-- user status is set back to empty.
toxSelfSetStatusMessage :: ToxPtr -> BS.ByteString -> IO (Either ErrSetInfo Bool)
toxSelfSetStatusMessage tox statusMsg =
    BS.useAsCStringLen statusMsg $ \(statusMsgStr, statusMsgLen) ->
        callErrFun $ tox_self_set_status_message tox statusMsgStr (fromIntegral statusMsgLen)


-- | Write the status message set by tox_self_set_status_message to a byte array.
--
-- If no status message was set before calling this function, the status is
-- empty, and this function has no effect.
--
-- Call tox_self_get_status_message_size to find out how much memory to allocate for
-- the result.
--
-- @param status_message A valid memory location large enough to hold the
--   status message. If this parameter is NULL, the function has no effect.
toxSelfGetStatusMessage :: ToxPtr -> IO BS.ByteString
toxSelfGetStatusMessage tox = do
    statusMessageLen <- tox_self_get_status_message_size tox
    allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
        tox_self_get_status_message tox statusMessagePtr
        BS.packCStringLen (statusMessagePtr, fromIntegral statusMessageLen)


-- | Set the client's user status.
--
-- @param user_status One of the user statuses listed in the enumeration above.
toxSelfSetStatus :: ToxPtr -> UserStatus -> IO ()
toxSelfSetStatus tox userStatus =
    tox_self_set_status tox $ toCEnum userStatus


--------------------------------------------------------------------------------
--
-- :: Friend list management
--
--------------------------------------------------------------------------------


-- | Add a friend to the friend list and send a friend request.
--
-- A friend request message must be at least 1 byte long and at most
-- 'tox_max_friend_request_length'.
--
-- Friend numbers are unique identifiers used in all functions that operate on
-- friends. Once added, a friend number is stable for the lifetime of the Tox
-- object. After saving the state and reloading it, the friend numbers may not
-- be the same as before. Deleting a friend creates a gap in the friend number
-- set, which is filled by the next adding of a friend. Any pattern in friend
-- numbers should not be relied on.
--
-- If more than INT32_MAX friends are added, this function causes undefined
-- behaviour.
--
-- @param address The address of the friend (returned by tox_self_get_address of
--   the friend you wish to add) it must be 'tox_address_size' bytes.
-- @param message The message that will be sent along with the friend request.
-- @param length The length of the data byte array.
--
-- @return the friend number on success, UINT32_MAX on failure.
toxFriendAdd :: ToxPtr -> BS.ByteString -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
toxFriendAdd tox address message =
    BS.useAsCStringLen message $ \(msgStr, msgLen) ->
        BS.useAsCString address $ \addr' ->
            callErrFun $ tox_friend_add tox addr' msgStr (fromIntegral msgLen)

-- | Add a friend without sending a friend request.
--
-- This function is used to add a friend in response to a friend request. If the
-- client receives a friend request, it can be reasonably sure that the other
-- client added this client as a friend, eliminating the need for a friend
-- request.
--
-- This function is also useful in a situation where both instances are
-- controlled by the same entity, so that this entity can perform the mutual
-- friend adding. In this case, there is no need for a friend request, either.
--
-- @param public_key A byte array of length 'tox_public_key_size' containing the
--   Public Key (not the Address) of the friend to add.
--
-- @return the friend number on success, UINT32_MAX on failure.
-- @see tox_friend_add for a more detailed description of friend numbers.
toxFriendAddNorequest :: ToxPtr -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
toxFriendAddNorequest tox address =
    BS.useAsCString address $ \addr' ->
        callErrFun $ tox_friend_add_norequest tox addr'


-- | Remove a friend from the friend list.
--
-- This does not notify the friend of their deletion. After calling this
-- function, this client will appear offline to the friend and no communication
-- can occur between the two.
--
-- @param friend_number Friend number for the friend to be deleted.
--
-- @return true on success.
toxFriendDelete :: ToxPtr -> Word32 -> IO (Either ErrFriendDelete Bool)
toxFriendDelete tox fn = callErrFun $ tox_friend_delete tox fn


--------------------------------------------------------------------------------
--
-- :: Friend list queries
--
--------------------------------------------------------------------------------


-- | Return the friend number associated with that Public Key.
--
-- @return the friend number on success, UINT32_MAX on failure.
-- @param public_key A byte array containing the Public Key.
toxFriendByPublicKey :: ToxPtr -> BS.ByteString -> IO (Either ErrFriendByPublicKey Word32)
toxFriendByPublicKey tox address =
    BS.useAsCString address $ \addr' ->
        callErrFun $ tox_friend_by_public_key tox addr'

-- | Checks if a friend with the given friend number exists and returns true if
-- it does.
toxFriendExists :: ToxPtr -> Word32 -> IO Bool
toxFriendExists = tox_friend_exists

-- | Copy a list of valid friend numbers into an array.
--
-- Call tox_self_get_friend_list_size to determine the number of elements to
-- allocate.
--
-- @param list A memory region with enough space to hold the friend list. If
--   this parameter is 'nullPtr', this function has no effect.
toxSelfGetFriendList :: ToxPtr -> IO [Word32]
toxSelfGetFriendList tox = do
    friendListSize <- tox_self_get_friend_list_size tox
    allocaArray (fromIntegral friendListSize) $ \friendListPtr -> do
        tox_self_get_friend_list tox friendListPtr
        peekArray (fromIntegral friendListSize) friendListPtr


-- | Copies the Public Key associated with a given friend number to a byte
-- array.
--
-- @param friend_number The friend number you want the Public Key of.
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
--
-- @return true on success.
toxFriendGetPublicKey :: ToxPtr -> Word32 -> IO (Either ErrFriendGetPublicKey BS.ByteString)
toxFriendGetPublicKey tox fn =
    let pkLen = fromIntegral tox_public_key_size in
    allocaArray pkLen $ \pkPtr -> do
        nameRes <- callErrFun $ tox_friend_get_public_key tox fn pkPtr
        case nameRes of
            Left err -> return $ Left err
            Right _  -> Right <$> BS.packCStringLen (pkPtr, pkLen)


-- | Return a unix-time timestamp of the last time the friend associated with a given
-- friend number was seen online. This function will return UINT64_MAX on error.
--
-- @param friend_number The friend number you want to query.
toxFriendGetLastOnline :: ToxPtr -> Word32 -> IO (Either ErrFriendGetLastOnline EpochTime)
toxFriendGetLastOnline tox fn =
    callErrFun (tox_friend_get_last_online tox fn >=> (return . CTime . fromIntegral))


--------------------------------------------------------------------------------
--
-- :: Friend-specific state queries (can also be received through callbacks)
--
--------------------------------------------------------------------------------


-- | Write the name of the friend designated by the given friend number to a byte
-- array.
--
-- Call tox_friend_get_name_size to determine the allocation size for the `name`
-- parameter.
--
-- The data written to `name` is equal to the data received by the last
-- `friend_name` callback.
--
-- @param name A valid memory region large enough to store the friend's name.
--
-- @return true on success.
toxFriendGetName :: ToxPtr -> Word32 -> IO (Either ErrFriendQuery BS.ByteString)
toxFriendGetName tox fn = do
    nameLenRes <- callErrFun $ tox_friend_get_name_size tox fn
    case nameLenRes of
        Left err -> return $ Left err
        Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
            nameRes <- callErrFun $ tox_friend_get_name tox fn namePtr
            case nameRes of
                Left err -> return $ Left err
                Right _ -> Right <$> BS.packCStringLen (namePtr, fromIntegral nameLen)


-- | Write the status message of the friend designated by the given friend number to a byte
-- array.
--
-- Call tox_friend_get_status_message_size to determine the allocation size for the `status_name`
-- parameter.
--
-- The data written to `status_message` is equal to the data received by the last
-- `friend_status_message` callback.
--
-- @param status_message A valid memory region large enough to store the friend's status message.
toxFriendGetStatusMessage :: ToxPtr -> Word32 -> IO (Either ErrFriendQuery BS.ByteString)
toxFriendGetStatusMessage tox fn = do
    statusMessageLenRes <- callErrFun $ tox_friend_get_status_message_size tox fn
    case statusMessageLenRes of
        Left err -> return $ Left err
        Right statusMessageLen -> allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
            statusMessageRes <- callErrFun $ tox_friend_get_status_message tox fn statusMessagePtr
            case statusMessageRes of
                Left err -> return $ Left err
                Right _ -> Right <$> BS.packCStringLen (statusMessagePtr, fromIntegral statusMessageLen)


-- | Check whether a friend is currently connected to this client.
--
-- The result of this function is equal to the last value received by the
-- `friend_connection_status` callback.
--
-- @param friend_number The friend number for which to query the connection
--   status.
--
-- @return the friend's connection status as it was received through the
--   `friend_connection_status` event.
toxFriendGetConnectionStatus :: ToxPtr -> Word32 -> IO (Either ErrFriendQuery Connection)
toxFriendGetConnectionStatus tox fn =
    callErrFun (tox_friend_get_connection_status tox fn >=> return . fromCEnum)


-- | Check whether a friend is currently typing a message.
--
-- @param friend_number The friend number for which to query the typing status.
--
-- @return true if the friend is typing.
-- @return false if the friend is not typing, or the friend number was
--   invalid. Inspect the error code to determine which case it is.
toxFriendGetTyping :: ToxPtr -> Word32 -> IO (Either ErrFriendQuery Bool)
toxFriendGetTyping tox fn = callErrFun $ tox_friend_get_typing tox fn


--------------------------------------------------------------------------------
--
-- :: Sending private messages
--
--------------------------------------------------------------------------------


-- | Set the client's typing status for a friend.
--
-- The client is responsible for turning it on or off.
--
-- @param friend_number The friend to which the client is typing a message.
-- @param typing The typing status. True means the client is typing.
--
-- @return true on success.
toxSelfSetTyping :: ToxPtr -> Word32 -> Bool -> IO (Either ErrSetTyping Bool)
toxSelfSetTyping tox fn typing = callErrFun $ tox_self_set_typing tox fn typing


-- | Send a text chat message to an online friend.
--
-- This function creates a chat message packet and pushes it into the send
-- queue.
--
-- The message length may not exceed 'tox_max_message_length'. Larger messages
-- must be split by the client and sent as separate messages. Other clients can
-- then reassemble the fragments. Messages may not be empty.
--
-- The return value of this function is the message ID. If a read receipt is
-- received, the triggered `friend_read_receipt` event will be passed this
-- message ID.
--
-- Message IDs are unique per friend. The first message ID is 0. Message IDs are
-- incremented by 1 each time a message is sent. If UINT32_MAX messages were
-- sent, the next message ID is 0.
--
-- @param type Message type (normal, action, ...).
-- @param friend_number The friend number of the friend to send the message to.
-- @param message A non-'nullPtr' pointer to the first element of a byte array
--   containing the message text.
-- @param length Length of the message to be sent.
toxFriendSendMessage :: ToxPtr -> Word32 -> MessageType -> BS.ByteString -> IO (Either ErrFriendSendMessage Word32)
toxFriendSendMessage tox fn messageType message =
    BS.useAsCStringLen message $ \(msgStr, msgLen) ->
        callErrFun $ tox_friend_send_message tox fn (toCEnum messageType) msgStr (fromIntegral msgLen)


--------------------------------------------------------------------------------
--
-- :: File transmission: common between sending and receiving
--
--------------------------------------------------------------------------------


-- | Generates a cryptographic hash of the given data.
--
-- This function may be used by clients for any purpose, but is provided
-- primarily for validating cached avatars. This use is highly recommended to
-- avoid unnecessary avatar updates.
--
-- If hash is 'nullPtr' or data is 'nullPtr' while length is not 0 the function
-- returns false, otherwise it returns true.
--
-- This function is a wrapper to internal message-digest functions.
--
-- @param hash A valid memory location the hash data. It must be at least
--   'tox_hash_length' bytes in size.
-- @param data Data to be hashed or 'nullPtr'.
-- @param length Size of the data array or 0.
--
-- @return true if hash was not 'nullPtr'.
toxHash :: BS.ByteString -> IO BS.ByteString
toxHash d =
    let hashLen = fromIntegral tox_hash_length in
    allocaArray hashLen $ \hashPtr ->
        BS.useAsCStringLen d $ \(dataPtr, dataLen) -> do
            _ <- tox_hash hashPtr dataPtr (fromIntegral dataLen)
            BS.packCStringLen (dataPtr, fromIntegral dataLen)


-- | Sends a file control command to a friend for a given file transfer.
--
-- @param friend_number The friend number of the friend the file is being
--   transferred to or received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param control The control command to send.
--
-- @return true on success.
toxFileControl :: ToxPtr -> Word32 -> Word32 -> FileControl -> IO (Either ErrFileControl Bool)
toxFileControl tox fn fileNum control = callErrFun $ tox_file_control tox fn fileNum (toCEnum control)


-- | Sends a file seek control command to a friend for a given file transfer.
--
-- This function can only be called to resume a file transfer right before
-- FileControlResume is sent.
--
-- @param friend_number The friend number of the friend the file is being
--   received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param position The position that the file should be seeked to.
toxFileSeek :: ToxPtr -> Word32 -> Word32 -> Word64 -> IO (Either ErrFileSeek Bool)
toxFileSeek tox fn fileNum pos = callErrFun $ tox_file_seek tox fn fileNum pos


-- | Copy the file id associated to the file transfer to a byte array.
--
-- @param friend_number The friend number of the friend the file is being
--   transferred to or received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param file_id A memory region of at least 'tox_file_id_length' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
--
-- @return true on success.
toxFileGetFileId :: ToxPtr -> Word32 -> Word32 -> IO (Either ErrFileGet BS.ByteString)
toxFileGetFileId tox fn fileNum =
    let fileIdLen = fromIntegral tox_file_id_length in
    allocaArray fileIdLen $ \fileIdPtr -> do
        idRes <- callErrFun $ tox_file_get_file_id tox fn fileNum fileIdPtr
        case idRes of
            Left err -> return $ Left err
            Right _  -> Right <$> BS.packCStringLen (fileIdPtr, fileIdLen)


--------------------------------------------------------------------------------
--
-- :: File transmission: sending
--
--------------------------------------------------------------------------------


-- | Send a file transmission request.
--
-- Maximum filename length is 'tox_max_filename_length' bytes. The filename
-- should generally just be a file name, not a path with directory names.
--
-- If a non-UINT64_MAX file size is provided, it can be used by both sides to
-- determine the sending progress. File size can be set to UINT64_MAX for
-- streaming data of unknown size.
--
-- File transmission occurs in chunks, which are requested through the
-- `file_chunk_request` event.
--
-- When a friend goes offline, all file transfers associated with the friend are
-- purged from core.
--
-- If the file contents change during a transfer, the behaviour is unspecified
-- in general. What will actually happen depends on the mode in which the file
-- was modified and how the client determines the file size.
--
-- - If the file size was increased
--   - and sending mode was streaming (file_size = UINT64_MAX), the behaviour
--     will be as expected.
--   - and sending mode was file (file_size != UINT64_MAX), the
--     file_chunk_request callback will receive length = 0 when Core thinks
--     the file transfer has finished. If the client remembers the file size as
--     it was when sending the request, it will terminate the transfer normally.
--     If the client re-reads the size, it will think the friend cancelled the
--     transfer.
-- - If the file size was decreased
--   - and sending mode was streaming, the behaviour is as expected.
--   - and sending mode was file, the callback will return 0 at the new
--     (earlier) end-of-file, signalling to the friend that the transfer was
--     cancelled.
-- - If the file contents were modified
--   - at a position before the current read, the two files (local and remote)
--     will differ after the transfer terminates.
--   - at a position after the current read, the file transfer will succeed as
--     expected.
--   - In either case, both sides will regard the transfer as complete and
--     successful.
--
-- @param friend_number The friend number of the friend the file send request
--   should be sent to.
-- @param kind The meaning of the file to be sent.
-- @param file_size Size in bytes of the file the client wants to send,
--   UINT64_MAX if unknown or streaming.
-- @param file_id A file identifier of length 'tox_file_id_length' that can be
--   used to uniquely identify file transfers across core restarts. If
--   'nullPtr', a random one will be generated by core. It can then be obtained
--   by using tox_file_get_file_id().
-- @param filename Name of the file. Does not need to be the actual name. This
--   name will be sent along with the file send request.
-- @param filename_length Size in bytes of the filename.
--
-- @return A file number used as an identifier in subsequent callbacks. This
--   number is per friend. File numbers are reused after a transfer terminates.
--   On failure, this function returns UINT32_MAX. Any pattern in file numbers
--   should not be relied on.
toxFileSend :: ToxPtr -> Word32 -> FileKind -> Word64 -> BS.ByteString -> IO (Either ErrFileSend Word32)
toxFileSend tox fn fileKind fileSize fileName =
    BS.useAsCStringLen fileName $ \(fileNamePtr, fileNameLen) ->
        callErrFun $ tox_file_send tox fn (fromIntegral $ fromEnum fileKind) fileSize nullPtr fileNamePtr (fromIntegral fileNameLen)


-- | Send a chunk of file data to a friend.
--
-- This function is called in response to the `file_chunk_request` callback.
-- The length parameter should be equal to the one received though the
-- callback.  If it is zero, the transfer is assumed complete. For files with
-- known size, Core will know that the transfer is complete after the last byte
-- has been received, so it is not necessary (though not harmful) to send a
-- zero-length chunk to terminate. For streams, core will know that the
-- transfer is finished if a chunk with length less than the length requested
-- in the callback is sent.
--
-- @param friend_number The friend number of the receiving friend for this file.
-- @param file_number The file transfer identifier returned by tox_file_send.
-- @param position The file or stream position from which to continue reading.
-- @return true on success.
toxFileSendChunk :: ToxPtr -> Word32 -> Word32 -> Word64 -> BS.ByteString -> IO (Either ErrFileSendChunk Bool)
toxFileSendChunk tox fn fileNum pos d =
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ tox_file_send_chunk tox fn fileNum pos dataPtr (fromIntegral dataLen)


--------------------------------------------------------------------------------
--
-- :: Conference management
--
--------------------------------------------------------------------------------


-- | Creates a new conference.
--
-- This function creates a new text conference.
--
-- @return conference number on success, or UINT32_MAX on failure.
toxConferenceNew :: ToxPtr -> IO (Either ErrConferenceNew Word32)
toxConferenceNew tox = callErrFun $ tox_conference_new tox


-- | This function deletes a conference.
--
-- @param conference_number The conference number of the conference to be deleted.
--
-- @return true on success.
toxConferenceDelete :: ToxPtr -> Word32 -> IO (Either ErrConferenceDelete Bool)
toxConferenceDelete tox gn = callErrFun $ tox_conference_delete tox gn


-- | Return the number of peers in the conference. Return value is unspecified on failure.
toxConferencePeerCount :: ToxPtr -> Word32 -> IO (Either ErrConferencePeerQuery Word32)
toxConferencePeerCount tox gn = callErrFun $ tox_conference_peer_count tox gn


-- | Copy the name of peer_number who is in conference_number to name.
-- name must be at least TOX_MAX_NAME_LENGTH long.
--
-- @return true on success.
toxConferencePeerGetName :: ToxPtr -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
toxConferencePeerGetName tox gn pn = do
    nameLenRes <- callErrFun $ tox_conference_peer_get_name_size tox gn pn
    case nameLenRes of
        Left err -> return $ Left err
        Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
            nameRes <- callErrFun $ tox_conference_peer_get_name tox gn pn namePtr
            case nameRes of
                Left err -> return $ Left err
                Right _ -> Right <$> BS.packCStringLen (namePtr, fromIntegral nameLen)


-- | Copy the public key of peer_number who is in conference_number to public_key.
-- public_key must be TOX_PUBLIC_KEY_SIZE long.
--
-- @return true on success.
toxConferencePeerGetPublicKey :: ToxPtr -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
toxConferencePeerGetPublicKey tox gn pn =
    let pkLen = fromIntegral tox_public_key_size in
    allocaArray pkLen $ \pkPtr -> do
        nameRes <- callErrFun $ tox_conference_peer_get_public_key tox gn pn pkPtr
        case nameRes of
            Left err -> return $ Left err
            Right _  -> Right <$> BS.packCStringLen (pkPtr, pkLen)


-- | Return true if passed peer_number corresponds to our own.
toxConferencePeerNumberIsOurs :: ToxPtr -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery Bool)
toxConferencePeerNumberIsOurs tox gn pn = callErrFun $ tox_conference_peer_number_is_ours tox gn pn


-- | Invites a friend to a conference.
--
-- @param friend_number The friend number of the friend we want to invite.
-- @param conference_number The conference number of the conference we want to invite the friend to.
--
-- @return true on success.
toxConferenceInvite :: ToxPtr -> Word32 -> Word32 -> IO (Either ErrConferenceInvite Bool)
toxConferenceInvite tox fn gn = callErrFun $ tox_conference_invite tox fn gn


-- | Joins a conference that the client has been invited to.
--
-- @param friend_number The friend number of the friend who sent the invite.
-- @param cookie Received via the `conference_invite` event.
-- @param length The size of cookie.
--
-- @return conference number on success, UINT32_MAX on failure.
toxConferenceJoin :: ToxPtr -> Word32 -> BS.ByteString -> IO (Either ErrConferenceJoin Word32)
toxConferenceJoin tox fn cookie =
    BS.useAsCStringLen cookie $ \(cookiePtr, cookieLen) ->
        callErrFun $ tox_conference_join tox fn cookiePtr (fromIntegral cookieLen)


-- | Send a text chat message to the conference.
--
-- This function creates a conference message packet and pushes it into the send
-- queue.
--
-- The message length may not exceed TOX_MAX_MESSAGE_LENGTH. Larger messages
-- must be split by the client and sent as separate messages. Other clients can
-- then reassemble the fragments.
--
-- @param conference_number The conference number of the conference the message is intended for.
-- @param type Message type (normal, action, ...).
-- @param message A non-NULL pointer to the first element of a byte array
--   containing the message text.
-- @param length Length of the message to be sent.
--
-- @return true on success.
toxConferenceSendMessage :: ToxPtr -> Word32 -> MessageType -> BS.ByteString -> IO (Either ErrConferenceSendMessage Bool)
toxConferenceSendMessage tox gn messageType message =
    BS.useAsCStringLen message $ \(msgPtr, msgLen) ->
        callErrFun $ tox_conference_send_message tox gn (toCEnum messageType) msgPtr (fromIntegral msgLen)


-- | Write the title designated by the given conference number to a byte array.
--
-- Call tox_conference_get_title_size to determine the allocation size for the `title` parameter.
--
-- The data written to `title` is equal to the data received by the last
-- `conference_title` callback.
--
-- @param title A valid memory region large enough to store the title.
--   If this parameter is NULL, this function has no effect.
--
-- @return true on success.
toxConferenceGetTitle :: ToxPtr -> Word32 -> IO (Either ErrConferenceTitle BS.ByteString)
toxConferenceGetTitle tox gn = do
    titleLenRes <- callErrFun $ tox_conference_get_title_size tox gn
    case titleLenRes of
        Left err -> return $ Left err
        Right titleLen -> allocaArray (fromIntegral titleLen) $ \titlePtr -> do
            titleRes <- callErrFun $ tox_conference_get_title tox gn titlePtr
            case titleRes of
                Left err -> return $ Left err
                Right _ -> Right <$> BS.packCStringLen (titlePtr, fromIntegral titleLen)

-- | Set the conference title and broadcast it to the rest of the conference.
--
-- Title length cannot be longer than TOX_MAX_NAME_LENGTH.
--
-- @return true on success.
toxConferenceSetTitle :: ToxPtr -> Word32 -> BS.ByteString -> IO (Either ErrConferenceTitle Bool)
toxConferenceSetTitle tox gn title =
    BS.useAsCStringLen title $ \(titlePtr, titleLen) ->
        callErrFun $ tox_conference_set_title tox gn titlePtr (fromIntegral titleLen)


-- | Copy a list of valid conference IDs into the array chatlist. Determine how much space
-- to allocate for the array with the `tox_conference_get_chatlist_size` function.
toxConferenceGetChatlist :: ToxPtr -> IO [Word32]
toxConferenceGetChatlist tox = do
    chatListSize <- tox_conference_get_chatlist_size tox
    allocaArray (fromIntegral chatListSize) $ \chatListPtr -> do
        tox_conference_get_chatlist tox chatListPtr
        peekArray (fromIntegral chatListSize) chatListPtr

toxConferenceGetType :: ToxPtr -> Word32 -> IO (Either ErrConferenceGetType ConferenceType)
toxConferenceGetType tox gn = callErrFun (tox_conference_get_type tox gn >=> (return . fromCEnum))


--------------------------------------------------------------------------------
--
-- :: Low-level custom packet sending and receiving
--
--------------------------------------------------------------------------------


-- | Send a custom lossy packet to a friend.
--
-- The first byte of data must be in the range 200-254. Maximum length of a
-- custom packet is 'tox_max_custom_packet_size'.
--
-- Lossy packets behave like UDP packets, meaning they might never reach the
-- other side or might arrive more than once (if someone is messing with the
-- connection) or might arrive in the wrong order.
--
-- Unless latency is an issue, it is recommended that you use lossless custom
-- packets instead.
--
-- @param friend_number The friend number of the friend this lossy packet
--   should be sent to.
-- @param data A byte array containing the packet data.
-- @param length The length of the packet data byte array.
--
-- @return true on success.
toxFriendLossyPacket :: ToxPtr -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendLossyPacket tox fn d =
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ tox_friend_send_lossy_packet tox fn dataPtr (fromIntegral dataLen)

-- | Send a custom lossless packet to a friend.
--
-- The first byte of data must be in the range 160-191. Maximum length of a
-- custom packet is 'tox_max_custom_packet_size'.
--
-- Lossless packet behaviour is comparable to TCP (reliability, arrive in order)
-- but with packets instead of a stream.
--
-- @param friend_number The friend number of the friend this lossless packet
--   should be sent to.
-- @param data A byte array containing the packet data.
-- @param length The length of the packet data byte array.
--
-- @return true on success.
toxFriendLosslessPacket :: ToxPtr -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendLosslessPacket tox fn d =
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ tox_friend_send_lossless_packet tox fn dataPtr (fromIntegral dataLen)


--------------------------------------------------------------------------------
--
-- :: Low-level network information
--
--------------------------------------------------------------------------------


-- | Writes the temporary DHT public key of this instance to a byte array.
--
-- This can be used in combination with an externally accessible IP address and
-- the bound port (from tox_self_get_udp_port) to run a temporary bootstrap
-- node.
--
-- Be aware that every time a new instance is created, the DHT public key
-- changes, meaning this cannot be used to run a permanent bootstrap node.
--
-- @param dht_id A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
toxSelfGetDhtId :: ToxPtr -> IO BS.ByteString
toxSelfGetDhtId tox =
    let idLen = fromIntegral tox_public_key_size in
    allocaArray idLen $ \idPtr -> do
        tox_self_get_dht_id tox idPtr
        BS.packCStringLen (idPtr, idLen)

-- | Return the UDP port this Tox instance is bound to.
toxSelfGetUdpPort :: ToxPtr -> IO (Either ErrGetPort Word16)
toxSelfGetUdpPort tox = callErrFun $ tox_self_get_udp_port tox

-- | Return the TCP port this Tox instance is bound to. This is only relevant if
-- the instance is acting as a TCP relay.
toxSelfGetTcpPort :: ToxPtr -> IO (Either ErrGetPort Word16)
toxSelfGetTcpPort tox = callErrFun $ tox_self_get_tcp_port tox
