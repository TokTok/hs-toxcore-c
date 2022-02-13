{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}
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

import           Control.Exception       (bracket)
import           Control.Monad           ((>=>))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.MessagePack        as MP
import           Data.Word               (Word16, Word32, Word64)
import           Foreign.C.String        (CString, peekCStringLen, withCString,
                                          withCStringLen)
import           Foreign.C.Types         (CChar (..), CInt (..), CSize (..),
                                          CTime (..))
import           Foreign.ForeignPtr      (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc   (alloca)
import           Foreign.Marshal.Array   (allocaArray, peekArray)
import           Foreign.Ptr             (FunPtr, Ptr, nullPtr)
import           Foreign.Storable        (peek)
import           System.Posix.Types      (EpochTime)

import           Network.Tox.C.CEnum
import           Network.Tox.C.Constants
import           Network.Tox.C.Events
import           Network.Tox.C.Options
import           Network.Tox.C.Type


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


data ErrNew
    = ErrNewOk
      -- The function returned successfully.

    | ErrNewNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrNewMalloc
      -- The function was unable to allocate enough memory to store the internal
      -- structures for the Tox object.

    | ErrNewPortAlloc
      -- The function was unable to bind to a port. This may mean that all ports
      -- have already been bound, e.g. by other Tox instances, or it may mean a
      -- permission error. You may be able to gather more information from errno.

    | ErrNewProxyBadType
      -- proxy_type was invalid.

    | ErrNewProxyBadHost
      -- proxy_type was valid but the proxy_host passed had an invalid format or
      -- was 'nullPtr'.

    | ErrNewProxyBadPort
      -- proxy_type was valid, but the proxy_port was invalid.

    | ErrNewProxyNotFound
      -- The proxy address passed could not be resolved.

    | ErrNewLoadEncrypted
      -- The byte array to be loaded contained an encrypted save.

    | ErrNewLoadBadFormat
      -- The data format was invalid. This can happen when loading data that was
      -- saved by an older version of Tox, or when the data has been corrupted.
      -- When loading from badly formatted data, some data may have been loaded,
      -- and the rest is discarded. Passing an invalid length parameter also
      -- causes this error.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- @brief Creates and initialises a new Tox instance with the options passed.
--
-- This function will bring the instance into a valid state. Running the event
-- loop with a new instance will operate correctly.
--
-- If loading failed or succeeded only partially, the new or partially loaded
-- instance is returned and an error code is set.
--
-- @param options An options object as described above. If this parameter is
--   'nullPtr', the default options are used.
--
-- @see tox_iterate for the event loop.
--
-- @return A new Tox instance pointer on success or 'nullPtr' on failure.
foreign import ccall tox_new :: OptionsPtr -> CErr ErrNew -> IO ToxPtr

-- | Releases all resources associated with the Tox instance and disconnects
-- from the network.
--
-- After calling this function, the Tox pointer becomes invalid. No other
-- functions can be called, and the pointer value can no longer be read.
foreign import ccall "&tox_kill" tox_kill :: FunPtr (ToxPtr -> IO ())

toxNew :: Options -> IO (Either ErrNew Tox)
toxNew opts =
    fmap mapErr . withOptions opts $ \copts -> do
        result <- callErrFun . tox_new $ copts
        case result of
            Left err ->
                return $ Left err
            Right toxPtr -> do
                tox_events_init toxPtr
                Right <$> newForeignPtr tox_kill toxPtr

  where
    mapErr Left{}     = Left ErrNewMalloc
    mapErr (Right ok) = ok


-- | Calculates the number of bytes required to store the tox instance with
-- tox_get_savedata. This function cannot fail. The result is always greater
-- than 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_get_savedata_size :: ToxPtr -> IO CSize

-- | Store all information associated with the tox instance to a byte array.
--
-- @param data A memory region large enough to store the tox instance data.
--   Call tox_get_savedata_size to find the number of bytes required. If this
--   parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_get_savedata :: ToxPtr -> CString -> IO ()

toxGetSavedata :: Tox -> IO BS.ByteString
toxGetSavedata tox = withForeignPtr tox $ \toxPtr -> do
  savedataLen <- tox_get_savedata_size toxPtr
  allocaArray (fromIntegral savedataLen) $ \savedataPtr -> do
    tox_get_savedata toxPtr savedataPtr
    BS.packCStringLen (savedataPtr, fromIntegral savedataLen)


--------------------------------------------------------------------------------
--
-- :: Connection lifecycle and event loop
--
--------------------------------------------------------------------------------


data ErrBootstrap
    = ErrBootstrapOk
      -- The function returned successfully.

    | ErrBootstrapNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrBootstrapBadHost
      -- The address could not be resolved to an IP address, or the IP address
      -- passed was invalid.

    | ErrBootstrapBadPort
      -- The port passed was invalid. The valid port range is (1, 65535).
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_bootstrap :: ToxPtr -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ()

callBootstrapFun
  :: (ToxPtr -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ())
  -> Tox -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
callBootstrapFun f tox address port pubKey = withForeignPtr tox $ \toxPtr ->
  withCString address $ \address' ->
    BS.useAsCString pubKey $ \pubKey' ->
      callErrFun $ f toxPtr address' (fromIntegral port) pubKey'

toxBootstrap :: Tox -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
toxBootstrap = callBootstrapFun tox_bootstrap


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
foreign import ccall tox_add_tcp_relay :: ToxPtr -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ()

toxAddTcpRelay :: Tox -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
toxAddTcpRelay = callBootstrapFun tox_add_tcp_relay


-- | Common error codes for all functions that set a piece of user-visible
-- client information.
data ErrEventsIterate
    = ErrEventsIterateOk
      -- The function returned successfully.

    | ErrEventsIterateMalloc
      -- The function failed to allocate enough memory to store the events.

    | ErrEventsDecode
      -- Failed to encode or decode events in msgpack.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

foreign import ccall tox_events_init :: ToxPtr -> IO ()
foreign import ccall tox_events_iterate :: ToxPtr -> Bool -> CErr ErrEventsIterate -> IO ToxEvents
foreign import ccall tox_events_free :: ToxEvents -> IO ()

foreign import ccall tox_events_bytes_size :: ToxEvents -> IO Word32
foreign import ccall tox_events_get_bytes :: ToxEvents -> CString -> IO ()

foreign import ccall tox_events_load :: CString -> Word32 -> IO ToxEvents

toxEventsToPtr :: [Event] -> IO ToxEvents
toxEventsToPtr events =
    let encoded = MP.pack events in
    BS.useAsCStringLen (LBS.toStrict encoded) $ \(ptr, len) ->
        tox_events_load ptr (fromIntegral len)

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

toxEventsIterate :: Tox -> IO (Either String [Event])
toxEventsIterate tox =
    withForeignPtr tox $ \toxPtr -> callErrFun (tox_events_iterate toxPtr True) >>= \case
        Left err    -> return $ Left $ show err
        Right evPtr -> toxEventsFromPtr evPtr


-- | Return the time in milliseconds before tox_iterate() should be called again
-- for optimal performance.
foreign import ccall tox_iteration_interval :: ToxPtr -> IO Word32
toxIterationInterval :: Tox -> IO Word32
toxIterationInterval tox = withForeignPtr tox tox_iteration_interval

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
foreign import ccall tox_self_get_address :: ToxPtr -> CString -> IO ()

toxSelfGetAddress :: Tox -> IO BS.ByteString
toxSelfGetAddress tox = withForeignPtr tox $ \toxPtr ->
    let addrLen = fromIntegral tox_address_size in
    allocaArray addrLen $ \addrPtr -> do
        tox_self_get_address toxPtr addrPtr
        BS.packCStringLen (addrPtr, addrLen)

-- | Set the 4-byte nospam part of the address.
--
-- @param nospam Any 32 bit unsigned integer.
foreign import ccall tox_self_set_nospam :: ToxPtr -> Word32 -> IO ()
toxSelfSetNospam :: Tox -> Word32 -> IO ()
toxSelfSetNospam tox nospam = withForeignPtr tox $ \toxPtr -> tox_self_set_nospam toxPtr nospam

-- | Get the 4-byte nospam part of the address.
foreign import ccall tox_self_get_nospam :: ToxPtr -> IO Word32
toxSelfGetNospam :: Tox -> IO Word32
toxSelfGetNospam tox = withForeignPtr tox tox_self_get_nospam

-- | Copy the Tox Public Key (long term) from the Tox object.
--
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_public_key :: ToxPtr -> CString -> IO ()

toxSelfGetPublicKey :: Tox -> IO BS.ByteString
toxSelfGetPublicKey tox = withForeignPtr tox $ \toxPtr ->
    let pkLen = fromIntegral tox_public_key_size in
    allocaArray pkLen $ \pkPtr -> do
        tox_self_get_public_key toxPtr pkPtr
        BS.packCStringLen (pkPtr, pkLen)

-- | Copy the Tox Secret Key from the Tox object.
--
-- @param secret_key A memory region of at least 'tox_secret_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_secret_key :: ToxPtr -> CString -> IO ()

toxSelfGetSecretKey :: Tox -> IO BS.ByteString
toxSelfGetSecretKey tox = withForeignPtr tox $ \toxPtr ->
    let skLen = fromIntegral tox_secret_key_size in
    allocaArray skLen $ \skPtr -> do
        tox_self_get_secret_key toxPtr skPtr
        BS.packCStringLen (skPtr, skLen)


--------------------------------------------------------------------------------
--
-- :: User-visible client information (nickname/status)
--
--------------------------------------------------------------------------------


-- | Common error codes for all functions that set a piece of user-visible
-- client information.
data ErrSetInfo
    = ErrSetInfoOk
      -- The function returned successfully.

    | ErrSetInfoNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrSetInfoTooLong
      -- Information length exceeded maximum permissible size.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_self_set_name :: ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO ()
callSelfSetNameFun :: (ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO ()) ->
                       Tox -> String -> IO (Either ErrSetInfo ())
callSelfSetNameFun f tox name =
    withForeignPtr tox $ \toxPtr ->
        withCStringLen name $ \(nameStr, nameLen) ->
            callErrFun $ f toxPtr nameStr (fromIntegral nameLen)

toxSelfSetName :: Tox -> String -> IO (Either ErrSetInfo ())
toxSelfSetName = callSelfSetNameFun tox_self_set_name


-- | Return the length of the current nickname as passed to tox_self_set_name.
--
-- If no nickname was set before calling this function, the name is empty,
-- and this function returns 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_self_get_name_size :: ToxPtr -> IO CSize

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
foreign import ccall tox_self_get_name :: ToxPtr -> CString -> IO ()

toxSelfGetName :: Tox -> IO String
toxSelfGetName tox = withForeignPtr tox $ \toxPtr -> do
    nameLen <- tox_self_get_name_size toxPtr
    allocaArray (fromIntegral nameLen) $ \namePtr -> do
        tox_self_get_name toxPtr namePtr
        peekCStringLen (namePtr, fromIntegral nameLen)


-- | Set the client's status message.
--
-- Status message length cannot exceed 'tox_max_status_message_length'. If
-- length is 0, the status parameter is ignored (it can be 'nullPtr'), and the
-- user status is set back to empty.
foreign import ccall tox_self_set_status_message :: ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO ()
callSelfSetStatusMessageFun :: (ToxPtr -> CString -> CSize -> CErr ErrSetInfo -> IO ()) ->
                                Tox -> String -> IO (Either ErrSetInfo ())
callSelfSetStatusMessageFun f tox statusMsg = withForeignPtr tox $ \toxPtr ->
    withCStringLen statusMsg $ \(statusMsgStr, statusMsgLen) ->
        callErrFun $ f toxPtr statusMsgStr (fromIntegral statusMsgLen)

toxSelfSetStatusMessage :: Tox -> String -> IO (Either ErrSetInfo ())
toxSelfSetStatusMessage = callSelfSetStatusMessageFun tox_self_set_status_message


-- | Return the length of the current status message as passed to tox_self_set_status_message.
--
-- If no status message was set before calling this function, the status
-- is empty, and this function returns 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_self_get_status_message_size :: ToxPtr -> IO CSize


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
foreign import ccall tox_self_get_status_message :: ToxPtr -> CString -> IO ()

toxSelfGetStatusMessage :: Tox -> IO String
toxSelfGetStatusMessage tox = withForeignPtr tox $ \toxPtr -> do
    statusMessageLen <- tox_self_get_status_message_size toxPtr
    allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
        tox_self_get_status_message toxPtr statusMessagePtr
        peekCStringLen (statusMessagePtr, fromIntegral statusMessageLen)


-- | Set the client's user status.
--
-- @param user_status One of the user statuses listed in the enumeration above.
foreign import ccall tox_self_set_status :: ToxPtr -> CEnum UserStatus -> IO ()
toxSelfSetStatus :: Tox -> UserStatus -> IO ()
toxSelfSetStatus tox userStatus = withForeignPtr tox $ \toxPtr ->
    tox_self_set_status toxPtr $ toCEnum userStatus


--------------------------------------------------------------------------------
--
-- :: Friend list management
--
--------------------------------------------------------------------------------


data ErrFriendAdd
    = ErrFriendAddOk
      -- The function returned successfully.

    | ErrFriendAddNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFriendAddTooLong
      -- The length of the friend request message exceeded
      -- 'tox_max_friend_request_length'.

    | ErrFriendAddNoMessage
      -- The friend request message was empty. This, and the TooLong code will
      -- never be returned from tox_friend_add_norequest.

    | ErrFriendAddOwnKey
      -- The friend address belongs to the sending client.

    | ErrFriendAddAlreadySent
      -- A friend request has already been sent, or the address belongs to a
      -- friend that is already on the friend list.

    | ErrFriendAddBadChecksum
      -- The friend address checksum failed.

    | ErrFriendAddSetNewNospam
      -- The friend was already there, but the nospam value was different.

    | ErrFriendAddMalloc
      -- A memory allocation failed when trying to increase the friend list size.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_friend_add :: ToxPtr -> CString -> CString -> CSize -> CErr ErrFriendAdd -> IO Word32
callFriendAddFun :: (ToxPtr -> CString -> CString -> CSize -> CErr ErrFriendAdd -> IO Word32) ->
                     Tox -> BS.ByteString -> String -> IO (Either ErrFriendAdd Word32)
callFriendAddFun f tox address message = withForeignPtr tox $ \toxPtr ->
    withCStringLen message $ \(msgStr, msgLen) ->
        BS.useAsCString address $ \addr' ->
            callErrFun $ f toxPtr addr' msgStr (fromIntegral msgLen)

toxFriendAdd :: Tox -> BS.ByteString -> String -> IO (Either ErrFriendAdd Word32)
toxFriendAdd = callFriendAddFun tox_friend_add

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
foreign import ccall tox_friend_add_norequest :: ToxPtr -> CString -> CErr ErrFriendAdd -> IO Word32
callFriendAddNorequestFun :: (ToxPtr -> CString -> CErr ErrFriendAdd -> IO Word32) ->
                              Tox -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
callFriendAddNorequestFun f tox address = withForeignPtr tox $ \toxPtr ->
    BS.useAsCString address $ \addr' ->
        callErrFun $ f toxPtr addr'

toxFriendAddNorequest :: Tox -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
toxFriendAddNorequest = callFriendAddNorequestFun tox_friend_add_norequest


data ErrFriendDelete
    = ErrFriendDeleteOk
      -- The function returned successfully.

    | ErrFriendDeleteFriendNotFound
      -- There was no friend with the given friend number. No friends were
      -- deleted.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Remove a friend from the friend list.
--
-- This does not notify the friend of their deletion. After calling this
-- function, this client will appear offline to the friend and no communication
-- can occur between the two.
--
-- @param friend_number Friend number for the friend to be deleted.
--
-- @return true on success.
foreign import ccall tox_friend_delete :: ToxPtr -> Word32 -> CErr ErrFriendDelete -> IO ()

toxFriendDelete :: Tox -> Word32 -> IO (Either ErrFriendDelete ())
toxFriendDelete tox fn = withForeignPtr tox $ \toxPtr -> callErrFun $ tox_friend_delete toxPtr fn


--------------------------------------------------------------------------------
--
-- :: Friend list queries
--
--------------------------------------------------------------------------------


data ErrFriendByPublicKey
    = ErrFriendByPublicKeyOk
      -- The function returned successfully.

    | ErrFriendByPublicKeyNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFriendByPublicKeyNotFound
      -- No friend with the given Public Key exists on the friend list.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return the friend number associated with that Public Key.
--
-- @return the friend number on success, UINT32_MAX on failure.
-- @param public_key A byte array containing the Public Key.
foreign import ccall tox_friend_by_public_key :: ToxPtr -> CString -> CErr ErrFriendByPublicKey -> IO Word32
callFriendByPublicKey :: (ToxPtr -> CString -> CErr ErrFriendByPublicKey -> IO Word32) ->
                          Tox -> BS.ByteString -> IO (Either ErrFriendByPublicKey Word32)
callFriendByPublicKey f tox address = withForeignPtr tox $ \toxPtr ->
    BS.useAsCString address $ \addr' ->
        callErrFun $ f toxPtr addr'

toxFriendByPublicKey :: Tox -> BS.ByteString -> IO (Either ErrFriendByPublicKey Word32)
toxFriendByPublicKey = callFriendByPublicKey tox_friend_by_public_key

-- | Checks if a friend with the given friend number exists and returns true if
-- it does.
foreign import ccall tox_friend_exists :: ToxPtr -> Word32 -> IO Bool
toxFriendExists :: Tox -> Word32 -> IO Bool
toxFriendExists tox num = withForeignPtr tox $ \toxPtr -> tox_friend_exists toxPtr num

-- | Return the number of friends on the friend list.
--
-- This function can be used to determine how much memory to allocate for
-- tox_self_get_friend_list.
foreign import ccall tox_self_get_friend_list_size :: ToxPtr -> IO CSize

-- | Copy a list of valid friend numbers into an array.
--
-- Call tox_self_get_friend_list_size to determine the number of elements to
-- allocate.
--
-- @param list A memory region with enough space to hold the friend list. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_friend_list :: ToxPtr -> Ptr Word32 -> IO ()

toxSelfGetFriendList :: Tox -> IO [Word32]
toxSelfGetFriendList tox = withForeignPtr tox $ \toxPtr -> do
    friendListSize <- tox_self_get_friend_list_size toxPtr
    allocaArray (fromIntegral friendListSize) $ \friendListPtr -> do
        tox_self_get_friend_list toxPtr friendListPtr
        peekArray (fromIntegral friendListSize) friendListPtr

data ErrFriendGetPublicKey
    = ErrFriendGetPublicKeyOk
      -- The function returned successfully.

    | ErrFriendGetPublicKeyFriendNotFound
      -- No friend with the given number exists on the friend list.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Copies the Public Key associated with a given friend number to a byte
-- array.
--
-- @param friend_number The friend number you want the Public Key of.
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
--
-- @return true on success.
foreign import ccall tox_friend_get_public_key :: ToxPtr -> Word32 -> CString -> CErr ErrFriendGetPublicKey -> IO Bool
callFriendGetPublicKey :: (ToxPtr -> Word32 -> CString -> CErr ErrFriendGetPublicKey -> IO Bool) ->
                           Tox -> Word32 -> IO (Either ErrFriendGetPublicKey BS.ByteString)
callFriendGetPublicKey f tox fn = withForeignPtr tox $ \toxPtr ->
    let pkLen = fromIntegral tox_public_key_size in
    alloca $ \errPtr ->
        allocaArray pkLen $ \pkPtr -> do
            _ <- f toxPtr fn pkPtr errPtr
            callGetPublicKey errPtr pkPtr pkLen

callGetPublicKey
  :: (Bounded err, Enum err, Eq err)
  => Ptr (CEnum err)
  -> Ptr CChar
  -> Int
  -> IO (Either err BS.ByteString)
callGetPublicKey errPtr pkPtr pkLen = do
    err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
    str <- BS.packCStringLen (pkPtr, pkLen)
    return $ if err /= minBound
             then Left  err
             else Right str

toxFriendGetPublicKey :: Tox -> Word32 -> IO (Either ErrFriendGetPublicKey BS.ByteString)
toxFriendGetPublicKey = callFriendGetPublicKey tox_friend_get_public_key


data ErrFriendGetLastOnline
    = ErrFriendGetLastOnlineOk
      -- The function returned successfully.

    | ErrFriendGetLastOnlineFriendNotFound
      -- No friend with the given number exists on the friend list.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return a unix-time timestamp of the last time the friend associated with a given
-- friend number was seen online. This function will return UINT64_MAX on error.
--
-- @param friend_number The friend number you want to query.
foreign import ccall tox_friend_get_last_online :: ToxPtr -> Word32 -> CErr ErrFriendGetLastOnline -> IO Word64
callFriendGetLastOnline :: (ToxPtr -> Word32 -> CErr ErrFriendGetLastOnline -> IO Word64) ->
                            Tox -> Word32 -> IO (Either ErrFriendGetLastOnline EpochTime)
callFriendGetLastOnline f tox fn = withForeignPtr tox $ \toxPtr ->
    callErrFun (f toxPtr fn >=> (return . CTime . fromIntegral))

toxFriendGetLastOnline :: Tox -> Word32 -> IO (Either ErrFriendGetLastOnline EpochTime)
toxFriendGetLastOnline = callFriendGetLastOnline tox_friend_get_last_online


--------------------------------------------------------------------------------
--
-- :: Friend-specific state queries (can also be received through callbacks)
--
--------------------------------------------------------------------------------


-- | Common error codes for friend state query functions.
data ErrFriendQuery
    = ErrFriendQueryOk
      -- The function returned successfully.

    | ErrFriendQueryNull
      -- The pointer parameter for storing the query result (name, message) was
      -- NULL. Unlike the `_self_` variants of these functions, which have no effect
      -- when a parameter is NULL, these functions return an error in that case.

    | ErrFriendQueryFriendNotFound
      -- The friend_number did not designate a valid friend.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return the length of the friend's name. If the friend number is invalid, the
-- return value is unspecified.
--
-- The return value is equal to the `length` argument received by the last
-- `friend_name` callback.
foreign import ccall tox_friend_get_name_size :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO CSize

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
foreign import ccall tox_friend_get_name :: ToxPtr -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool

toxFriendGetName :: Tox -> Word32 -> IO (Either ErrFriendQuery String)
toxFriendGetName tox fn = withForeignPtr tox $ \toxPtr -> do
    nameLenRes <- callErrFun $ tox_friend_get_name_size toxPtr fn
    case nameLenRes of
        Left err -> return $ Left err
        Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
            nameRes <- callErrFun $ tox_friend_get_name toxPtr fn namePtr
            case nameRes of
                Left err -> return $ Left err
                Right _ -> Right <$> peekCStringLen (namePtr, fromIntegral nameLen)


-- | Return the length of the friend's status message. If the friend number is
-- invalid, the return value is SIZE_MAX.
foreign import ccall tox_friend_get_status_message_size :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO CSize

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
foreign import ccall tox_friend_get_status_message :: ToxPtr -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool

toxFriendGetStatusMessage :: Tox -> Word32 -> IO (Either ErrFriendQuery String)
toxFriendGetStatusMessage tox fn = withForeignPtr tox $ \toxPtr -> do
    statusMessageLenRes <- callErrFun $ tox_friend_get_status_message_size toxPtr fn
    case statusMessageLenRes of
        Left err -> return $ Left err
        Right statusMessageLen -> allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
            statusMessageRes <- callErrFun $ tox_friend_get_status_message toxPtr fn statusMessagePtr
            case statusMessageRes of
                Left err -> return $ Left err
                Right _ -> Right <$> peekCStringLen (statusMessagePtr, fromIntegral statusMessageLen)


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
foreign import ccall tox_friend_get_connection_status :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO (CEnum Connection)

callFriendGetConnectionStatus :: (ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO (CEnum Connection)) ->
                                  Tox -> Word32 -> IO (Either ErrFriendQuery Connection)
callFriendGetConnectionStatus f tox fn = withForeignPtr tox $ \toxPtr -> callErrFun (f toxPtr fn >=> (return . fromCEnum))

toxFriendGetConnectionStatus :: Tox -> Word32 -> IO (Either ErrFriendQuery Connection)
toxFriendGetConnectionStatus = callFriendGetConnectionStatus tox_friend_get_connection_status


-- | Check whether a friend is currently typing a message.
--
-- @param friend_number The friend number for which to query the typing status.
--
-- @return true if the friend is typing.
-- @return false if the friend is not typing, or the friend number was
--   invalid. Inspect the error code to determine which case it is.
foreign import ccall tox_friend_get_typing :: ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO Bool

callFriendGetTyping :: (ToxPtr -> Word32 -> CErr ErrFriendQuery -> IO Bool) ->
                        Tox -> Word32 -> IO (Either ErrFriendQuery Bool)
callFriendGetTyping f tox fn = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr fn

toxFriendGetTyping :: Tox -> Word32 -> IO (Either ErrFriendQuery Bool)
toxFriendGetTyping = callFriendGetTyping tox_friend_get_typing


--------------------------------------------------------------------------------
--
-- :: Sending private messages
--
--------------------------------------------------------------------------------


data ErrSetTyping
    = ErrSetTypingOk
      -- The function returned successfully.

    | ErrSetTypingFriendNotFound
      -- The friend number did not designate a valid friend.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Set the client's typing status for a friend.
--
-- The client is responsible for turning it on or off.
--
-- @param friend_number The friend to which the client is typing a message.
-- @param typing The typing status. True means the client is typing.
--
-- @return true on success.
foreign import ccall tox_self_set_typing :: ToxPtr -> Word32 -> Bool -> CErr ErrSetTyping -> IO Bool
callSelfSetTyping :: (ToxPtr -> Word32 -> Bool -> CErr ErrSetTyping -> IO Bool) ->
                      Tox -> Word32 -> Bool -> IO (Either ErrSetTyping Bool)
callSelfSetTyping f tox fn typing = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr fn typing

toxSelfSetTyping :: Tox -> Word32 -> Bool -> IO (Either ErrSetTyping Bool)
toxSelfSetTyping = callSelfSetTyping tox_self_set_typing

data ErrFriendSendMessage
    = ErrFriendSendMessageOk
      -- The function returned successfully.

    | ErrFriendSendMessageNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFriendSendMessageFriendNotFound
      -- The friend number did not designate a valid friend.

    | ErrFriendSendMessageFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFriendSendMessageSendq
      -- An allocation error occurred while increasing the send queue size.

    | ErrFriendSendMessageTooLong
      -- Message length exceeded 'tox_max_message_length'.

    | ErrFriendSendMessageEmpty
      -- Attempted to send a zero-length message.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_friend_send_message :: ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrFriendSendMessage -> IO Word32
callFriendSendMessage :: (ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrFriendSendMessage -> IO Word32) ->
                          Tox -> Word32 -> MessageType -> BS.ByteString -> IO (Either ErrFriendSendMessage Word32)
callFriendSendMessage f tox fn messageType message = withForeignPtr tox $ \toxPtr -> 
    BS.useAsCStringLen message $ \(msgStr, msgLen) ->
        callErrFun $ f toxPtr fn (toCEnum messageType) msgStr (fromIntegral msgLen)

toxFriendSendMessage :: Tox -> Word32 -> MessageType -> BS.ByteString -> IO (Either ErrFriendSendMessage Word32)
toxFriendSendMessage = callFriendSendMessage tox_friend_send_message


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
foreign import ccall tox_hash :: CString -> CString -> CSize -> IO Bool

toxHash :: BS.ByteString -> IO BS.ByteString
toxHash d =
  let hashLen = fromIntegral tox_hash_length in
  allocaArray hashLen $ \hashPtr ->
    BS.useAsCStringLen d $ \(dataPtr, dataLen) -> do
      _ <- tox_hash hashPtr dataPtr (fromIntegral dataLen)
      BS.packCStringLen (dataPtr, fromIntegral dataLen)

data FileKind
  = FileKindData
    -- Arbitrary file data. Clients can choose to handle it based on the file
    -- name or magic or any other way they choose.

  | FileKindAvatar
    -- Avatar file_id. This consists of tox_hash(image).  Avatar data. This
    -- consists of the image data.
    --
    -- Avatars can be sent at any time the client wishes. Generally, a client
    -- will send the avatar to a friend when that friend comes online, and to
    -- all friends when the avatar changed. A client can save some traffic by
    -- remembering which friend received the updated avatar already and only
    -- send it if the friend has an out of date avatar.
    --
    -- Clients who receive avatar send requests can reject it (by sending
    -- FileControlCancel before any other controls), or accept it (by sending
    -- FileControlResume). The file_id of length 'tox_hash_length' bytes (same
    -- length as 'tox_file_id_length') will contain the hash. A client can
    -- compare this hash with a saved hash and send FileControlCancel to
    -- terminate the avatar transfer if it matches.
    --
    -- When file_size is set to 0 in the transfer request it means that the
    -- client has no avatar.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


data ErrFileControl
    = ErrFileControlOk
      -- The function returned successfully.

    | ErrFileControlFriendNotFound
      -- The friend_number passed did not designate a valid friend.

    | ErrFileControlFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFileControlNotFound
      -- No file transfer with the given file number was found for the given friend.

    | ErrFileControlNotPaused
      -- A Resume control was sent, but the file transfer is running normally.

    | ErrFileControlDenied
      -- A Resume control was sent, but the file transfer was paused by the other
      -- party. Only the party that paused the transfer can resume it.

    | ErrFileControlAlreadyPaused
      -- A Pause control was sent, but the file transfer was already paused.

    | ErrFileControlSendq
      -- Packet queue is full.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Sends a file control command to a friend for a given file transfer.
--
-- @param friend_number The friend number of the friend the file is being
--   transferred to or received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param control The control command to send.
--
-- @return true on success.
foreign import ccall tox_file_control :: ToxPtr -> Word32 -> Word32 -> CEnum FileControl -> CErr ErrFileControl -> IO Bool

callFileControl :: (ToxPtr -> Word32 -> Word32 -> CEnum FileControl -> CErr ErrFileControl -> IO Bool) ->
                    Tox -> Word32 -> Word32 -> FileControl -> IO (Either ErrFileControl Bool)
callFileControl f tox fn fileNum control = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr fn fileNum (toCEnum control)

toxFileControl :: Tox -> Word32 -> Word32 -> FileControl -> IO (Either ErrFileControl Bool)
toxFileControl = callFileControl tox_file_control

data ErrFileSeek
    = ErrFileSeekOk
      -- The function returned successfully.

    | ErrFileSeekFriendNotFound
      -- The friend_number passed did not designate a valid friend.

    | ErrFileSeekFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFileSeekNotFound
      -- No file transfer with the given file number was found for the given friend.

    | ErrFileSeekDenied
      -- File was not in a state where it could be seeked.

    | ErrFileSeekInvalidPosition
      -- Seek position was invalid

    | ErrFileSeekSendq
      -- Packet queue is full.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Sends a file seek control command to a friend for a given file transfer.
--
-- This function can only be called to resume a file transfer right before
-- FileControlResume is sent.
--
-- @param friend_number The friend number of the friend the file is being
--   received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param position The position that the file should be seeked to.
foreign import ccall tox_file_seek :: ToxPtr -> Word32 -> Word32 -> Word64 -> CErr ErrFileSeek -> IO Bool

callFileSeek :: (ToxPtr -> Word32 -> Word32 -> Word64 -> CErr ErrFileSeek -> IO Bool) ->
                 Tox -> Word32 -> Word32 -> Word64 -> IO (Either ErrFileSeek Bool)
callFileSeek f tox fn fileNum pos = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr fn fileNum pos

toxFileSeek :: Tox -> Word32 -> Word32 -> Word64 -> IO (Either ErrFileSeek Bool)
toxFileSeek = callFileSeek tox_file_seek


data ErrFileGet
    = ErrFileGetOk
      -- The function returned successfully.

    | ErrFileGetNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFileGetFriendNotFound
      -- The friend_number passed did not designate a valid friend.

    | ErrFileGetNotFound
      -- No file transfer with the given file number was found for the given friend.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Copy the file id associated to the file transfer to a byte array.
--
-- @param friend_number The friend number of the friend the file is being
--   transferred to or received from.
-- @param file_number The friend-specific identifier for the file transfer.
-- @param file_id A memory region of at least 'tox_file_id_length' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
--
-- @return true on success.
foreign import ccall tox_file_get_file_id :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrFileGet -> IO Bool

callFileGetFileId :: (ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrFileGet -> IO Bool) ->
                      Tox -> Word32 -> Word32 -> IO (Either ErrFileGet BS.ByteString)
callFileGetFileId f tox fn fileNum = withForeignPtr tox $ \toxPtr ->
    let fileIdLen = fromIntegral tox_file_id_length in
    alloca $ \errPtr ->
        allocaArray fileIdLen $ \fileIdPtr -> do
            _ <- f toxPtr fn fileNum fileIdPtr errPtr
            err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
            fileId <- BS.packCStringLen (fileIdPtr, fileIdLen)
            return $ if err /= minBound
                     then Left  err
                     else Right fileId

toxFileGetFileId :: Tox -> Word32 -> Word32 -> IO (Either ErrFileGet BS.ByteString)
toxFileGetFileId = callFileGetFileId tox_file_get_file_id


--------------------------------------------------------------------------------
--
-- :: File transmission: sending
--
--------------------------------------------------------------------------------


data ErrFileSend
    = ErrFileSendOk
      -- The function returned successfully.

    | ErrFileSendNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFileSendFriendNotFound
      -- The friend_number passed did not designate a valid friend.

    | ErrFileSendFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFileSendNameTooLong
      -- Filename length exceeded 'tox_max_filename_length' bytes.

    | ErrFileSendTooMany
      -- Too many ongoing transfers. The maximum number of concurrent file transfers
      -- is 256 per friend per direction (sending and receiving).
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_file_send :: ToxPtr -> Word32 -> CEnum FileKind -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32
callFileSend :: (ToxPtr -> Word32 -> CEnum FileKind -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32) ->
                 Tox -> Word32 -> FileKind -> Word64 -> String -> IO (Either ErrFileSend Word32)
callFileSend f tox fn fileKind fileSize fileName = withForeignPtr tox $ \toxPtr ->
    withCStringLen fileName $ \(fileNamePtr, fileNameLen) ->
        callErrFun $ f toxPtr fn (toCEnum fileKind) fileSize nullPtr fileNamePtr (fromIntegral fileNameLen)

toxFileSend :: Tox -> Word32 -> FileKind -> Word64 -> String -> IO (Either ErrFileSend Word32)
toxFileSend = callFileSend tox_file_send

data ErrFileSendChunk
    = ErrFileSendChunkOk
      -- The function returned successfully.

    | ErrFileSendChunkNull
      -- The length parameter was non-zero, but data was 'nullPtr'.

    | ErrFileSendChunkFriendNotFound
      -- The friend_number passed did not designate a valid friend.

    | ErrFileSendChunkFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFileSendChunkNotFound
      -- No file transfer with the given file number was found for the given friend.

    | ErrFileSendChunkNotTransferring
      -- File transfer was found but isn't in a transferring state: (paused, done,
      -- broken, etc...) (happens only when not called from the request chunk
      -- callback).

    | ErrFileSendChunkInvalidLength
      -- Attempted to send more or less data than requested. The requested data
      -- size is adjusted according to maximum transmission unit and the expected
      -- end of the file. Trying to send less or more than requested will return
      -- this error.

    | ErrFileSendChunkSendq
      -- Packet queue is full.

    | ErrFileSendChunkWrongPosition
      -- Position parameter was wrong.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_file_send_chunk :: ToxPtr -> Word32 -> Word32 -> Word64 -> CString -> CSize -> CErr ErrFileSendChunk -> IO Bool
callFileSendChunk :: (ToxPtr -> Word32 -> Word32 -> Word64 -> CString -> CSize -> CErr ErrFileSendChunk -> IO Bool) ->
                      Tox -> Word32 -> Word32 -> Word64 -> BS.ByteString ->  IO (Either ErrFileSendChunk Bool)
callFileSendChunk f tox fn fileNum pos d = withForeignPtr tox $ \toxPtr ->
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ f toxPtr fn fileNum pos dataPtr (fromIntegral dataLen)

toxFileSendChunk :: Tox -> Word32 -> Word32 -> Word64 -> BS.ByteString -> IO (Either ErrFileSendChunk Bool)
toxFileSendChunk = callFileSendChunk tox_file_send_chunk


--------------------------------------------------------------------------------
--
-- :: Conference management
--
--------------------------------------------------------------------------------


data ErrConferenceNew
    = ErrConferenceNewOk
      -- The function returned successfully.

    | ErrConferenceNewInit
      -- The conference instance failed to initialize.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Creates a new conference.
--
-- This function creates a new text conference.
--
-- @return conference number on success, or UINT32_MAX on failure.
foreign import ccall tox_conference_new :: ToxPtr -> CErr ErrConferenceNew -> IO Word32
callConferenceNew :: (ToxPtr -> CErr ErrConferenceNew -> IO Word32) ->
                      Tox -> IO (Either ErrConferenceNew Word32)
callConferenceNew f tox = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr

toxConferenceNew :: Tox -> IO (Either ErrConferenceNew Word32)
toxConferenceNew = callConferenceNew tox_conference_new


data ErrConferenceDelete
    = ErrConferenceDeleteOk
      -- The function returned successfully.

    | ErrConferenceDeleteConferenceNotFound
      -- The conference number passed did not designate a valid conference.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | This function deletes a conference.
--
-- @param conference_number The conference number of the conference to be deleted.
--
-- @return true on success.
foreign import ccall tox_conference_delete :: ToxPtr -> Word32 -> CErr ErrConferenceDelete -> IO Bool
callConferenceDelete :: (ToxPtr -> Word32 -> CErr ErrConferenceDelete -> IO Bool) ->
                         Tox -> Word32 -> IO (Either ErrConferenceDelete Bool)
callConferenceDelete f tox gn = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr gn

toxConferenceDelete :: Tox -> Word32 -> IO (Either ErrConferenceDelete Bool)
toxConferenceDelete = callConferenceDelete tox_conference_delete


-- | Error codes for peer info queries.
data ErrConferencePeerQuery
    = ErrConferencePeerQueryOk
      -- The function returned successfully.

    | ErrConferencePeerQueryConferenceNotFound
      -- The conference number passed did not designate a valid conference.

    | ErrConferencePeerQueryPeerNotFound
      -- The peer number passed did not designate a valid peer.

    | ErrConferencePeerQueryNoConnection
      -- The client is not connected to the conference.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)



-- | Return the number of peers in the conference. Return value is unspecified on failure.
foreign import ccall tox_conference_peer_count :: ToxPtr -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32
callConferencePeerCount :: (ToxPtr -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32) ->
                            Tox -> Word32 -> IO (Either ErrConferencePeerQuery Word32)
callConferencePeerCount f tox gn = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr gn

toxConferencePeerCount :: Tox -> Word32 -> IO (Either ErrConferencePeerQuery Word32)
toxConferencePeerCount = callConferencePeerCount tox_conference_peer_count


-- | Return the length of the peer's name. Return value is unspecified on failure.
foreign import ccall tox_conference_peer_get_name_size :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO CSize


-- | Copy the name of peer_number who is in conference_number to name.
-- name must be at least TOX_MAX_NAME_LENGTH long.
--
-- @return true on success.
foreign import ccall tox_conference_peer_get_name :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool

toxConferencePeerGetName :: Tox -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery String)
toxConferencePeerGetName tox gn pn = do
    nameLenRes <- withForeignPtr tox $ \toxPtr -> callErrFun $ tox_conference_peer_get_name_size toxPtr gn pn
    case nameLenRes of
        Left err -> return $ Left err
        Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
            nameRes <- withForeignPtr tox $ \toxPtr -> callErrFun $ tox_conference_peer_get_name toxPtr gn pn namePtr
            case nameRes of
                Left err -> return $ Left err
                Right _ -> Right <$> peekCStringLen (namePtr, fromIntegral nameLen)


-- | Copy the public key of peer_number who is in conference_number to public_key.
-- public_key must be TOX_PUBLIC_KEY_SIZE long.
--
-- @return true on success.
foreign import ccall tox_conference_peer_get_public_key :: ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
callConferencePeerGetPublicKey :: (ToxPtr -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool) ->
                                   Tox -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
callConferencePeerGetPublicKey f tox gn pn = withForeignPtr tox $ \toxPtr ->
    let pkLen = fromIntegral tox_public_key_size in
    alloca $ \errPtr ->
        allocaArray pkLen $ \pkPtr -> do
            _ <- f toxPtr gn pn pkPtr errPtr
            callGetPublicKey errPtr pkPtr pkLen

toxConferencePeerGetPublicKey :: Tox -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
toxConferencePeerGetPublicKey = callConferencePeerGetPublicKey tox_conference_peer_get_public_key


-- | Return true if passed peer_number corresponds to our own.
foreign import ccall tox_conference_peer_number_is_ours :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Bool
callConferencePeerNumberIsOurs :: (ToxPtr -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Bool) ->
                                   Tox -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery Bool)
callConferencePeerNumberIsOurs f tox gn pn = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr gn pn

toxConferencePeerNumberIsOurs :: Tox -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery Bool)
toxConferencePeerNumberIsOurs = callConferencePeerNumberIsOurs tox_conference_peer_number_is_ours


data ErrConferenceInvite
    = ErrConferenceInviteOk
      -- The function returned successfully.

    | ErrConferenceInviteConferenceNotFound
      -- The conference number passed did not designate a valid conference.

    | ErrConferenceInviteFailSend
      -- The invite packet failed to send.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)



-- | Invites a friend to a conference.
--
-- @param friend_number The friend number of the friend we want to invite.
-- @param conference_number The conference number of the conference we want to invite the friend to.
--
-- @return true on success.
foreign import ccall tox_conference_invite :: ToxPtr -> Word32 -> Word32 -> CErr ErrConferenceInvite -> IO Bool
callConferenceInvite :: (ToxPtr -> Word32 -> Word32 -> CErr ErrConferenceInvite -> IO Bool) ->
                         Tox -> Word32 -> Word32 -> IO (Either ErrConferenceInvite Bool)
callConferenceInvite f tox fn gn = withForeignPtr tox $ \toxPtr -> callErrFun $ f toxPtr fn gn

toxConferenceInvite :: Tox -> Word32 -> Word32 -> IO (Either ErrConferenceInvite Bool)
toxConferenceInvite = callConferenceInvite tox_conference_invite


data ErrConferenceJoin
    = ErrConferenceJoinOk
      -- The function returned successfully.

    | ErrConferenceJoinInvalidLength
      -- The cookie passed has an invalid length.

    | ErrConferenceJoinWrongType
      -- The conference is not the expected type. This indicates an invalid cookie.

    | ErrConferenceJoinFriendNotFound
      -- The friend number passed does not designate a valid friend.

    | ErrConferenceJoinDuplicate
      -- Client is already in this conference.

    | ErrConferenceJoinInitFail
      -- Conference instance failed to initialize.

    | ErrConferenceJoinFailSend
      -- The join packet failed to send.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)



-- | Joins a conference that the client has been invited to.
--
-- @param friend_number The friend number of the friend who sent the invite.
-- @param cookie Received via the `conference_invite` event.
-- @param length The size of cookie.
--
-- @return conference number on success, UINT32_MAX on failure.
foreign import ccall tox_conference_join :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceJoin -> IO Word32
callConferenceJoin :: (ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceJoin -> IO Word32) ->
                       Tox -> Word32 -> BS.ByteString -> IO (Either ErrConferenceJoin Word32)
callConferenceJoin f tox fn cookie = withForeignPtr tox $ \toxPtr ->
    BS.useAsCStringLen cookie $ \(cookiePtr, cookieLen) ->
        callErrFun $ f toxPtr fn cookiePtr (fromIntegral cookieLen)

toxConferenceJoin :: Tox -> Word32 -> BS.ByteString -> IO (Either ErrConferenceJoin Word32)
toxConferenceJoin = callConferenceJoin tox_conference_join


data ErrConferenceSendMessage
    = ErrConferenceSendMessageOk
      -- The function returned successfully.

    | ErrConferenceSendMessageConferenceNotFound
      -- The conference number passed did not designate a valid conference.

    | ErrConferenceSendMessageTooLong
      -- The message is too long.

    | ErrConferenceSendMessageNoConnection
      -- The client is not connected to the conference.

    | ErrConferenceSendMessageFailSend
      -- The message packet failed to send.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)



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
foreign import ccall tox_conference_send_message :: ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrConferenceSendMessage -> IO Bool
callConferenceSendMessage :: (ToxPtr -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrConferenceSendMessage -> IO Bool) ->
                              Tox -> Word32 -> MessageType -> String -> IO (Either ErrConferenceSendMessage Bool)
callConferenceSendMessage f tox gn messageType message = withForeignPtr tox $ \toxPtr ->
    withCStringLen message $ \(msgPtr, msgLen) ->
        callErrFun $ f toxPtr gn (toCEnum messageType) msgPtr (fromIntegral msgLen)

toxConferenceSendMessage :: Tox -> Word32 -> MessageType -> String -> IO (Either ErrConferenceSendMessage Bool)
toxConferenceSendMessage = callConferenceSendMessage tox_conference_send_message


data ErrConferenceTitle
    = ErrConferenceTitleOk
      -- The function returned successfully.

    | ErrConferenceTitleConferenceNotFound
      -- The conference number passed did not designate a valid conference.

    | ErrConferenceTitleInvalidLength
      -- The title is too long or empty.

    | ErrConferenceTitleFailSend
      -- The title packet failed to send.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)



-- | Return the length of the conference title. Return value is unspecified on failure.
--
-- The return value is equal to the `length` argument received by the last
-- `conference_title` callback.
foreign import ccall tox_conference_get_title_size :: ToxPtr -> Word32 -> CErr ErrConferenceTitle -> IO CSize


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
foreign import ccall tox_conference_get_title :: ToxPtr -> Word32 -> CString -> CErr ErrConferenceTitle -> IO Bool

toxConferenceGetTitle :: Tox -> Word32 -> IO (Either ErrConferenceTitle String)
toxConferenceGetTitle tox gn = do
    titleLenRes <- withForeignPtr tox $ \toxPtr -> callErrFun $ tox_conference_get_title_size toxPtr gn
    case titleLenRes of
        Left err -> return $ Left err
        Right titleLen -> allocaArray (fromIntegral titleLen) $ \titlePtr -> do
            titleRes <- withForeignPtr tox $ \toxPtr -> callErrFun $ tox_conference_get_title toxPtr gn titlePtr
            case titleRes of
                Left err -> return $ Left err
                Right _ -> Right <$> peekCStringLen (titlePtr, fromIntegral titleLen)

-- | Set the conference title and broadcast it to the rest of the conference.
--
-- Title length cannot be longer than TOX_MAX_NAME_LENGTH.
--
-- @return true on success.
foreign import ccall tox_conference_set_title :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceTitle -> IO Bool
callConferenceSetTitle :: (ToxPtr -> Word32 -> CString -> CSize -> CErr ErrConferenceTitle -> IO Bool) ->
                           Tox -> Word32 -> String -> IO (Either ErrConferenceTitle Bool)
callConferenceSetTitle f tox gn title = withForeignPtr tox $ \toxPtr ->
    withCStringLen title $ \(titlePtr, titleLen) ->
        callErrFun $ f toxPtr gn titlePtr (fromIntegral titleLen)

toxConferenceSetTitle :: Tox -> Word32 -> String -> IO (Either ErrConferenceTitle Bool)
toxConferenceSetTitle = callConferenceSetTitle tox_conference_set_title


-- | Return the number of conferences in the Tox instance.
-- This should be used to determine how much memory to allocate for `tox_conference_get_chatlist`.
foreign import ccall tox_conference_get_chatlist_size :: ToxPtr -> IO CSize


-- | Copy a list of valid conference IDs into the array chatlist. Determine how much space
-- to allocate for the array with the `tox_conference_get_chatlist_size` function.
foreign import ccall tox_conference_get_chatlist :: ToxPtr -> Ptr Word32 -> IO ()

toxConferenceGetChatlist :: Tox -> IO [Word32]
toxConferenceGetChatlist tox = withForeignPtr tox $ \toxPtr -> do
    chatListSize <- tox_conference_get_chatlist_size toxPtr
    allocaArray (fromIntegral chatListSize) $ \chatListPtr -> do
        tox_conference_get_chatlist toxPtr chatListPtr
        peekArray (fromIntegral chatListSize) chatListPtr

-- | Returns the type of conference (TOX_CONFERENCE_TYPE) that conference_number is. Return value is
-- unspecified on failure.
data ErrConferenceGetType
    = ErrConferenceGetTypeOk
      -- The function returned successfully.

    | ErrConferenceGetTypeConferenceNotFound
      -- The conference number passed did not designate a valid conference.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

foreign import ccall tox_conference_get_type :: ToxPtr -> Word32 -> CErr ErrConferenceGetType -> IO (CEnum ConferenceType)
callConferenceGetType :: (ToxPtr -> Word32 -> CErr ErrConferenceGetType -> IO (CEnum ConferenceType)) ->
                          Tox -> Word32 -> IO (Either ErrConferenceGetType ConferenceType)
callConferenceGetType f tox gn = withForeignPtr tox $ \toxPtr -> callErrFun (f toxPtr gn >=> (return . fromCEnum))

toxConferenceGetType :: Tox -> Word32 -> IO (Either ErrConferenceGetType ConferenceType)
toxConferenceGetType = callConferenceGetType tox_conference_get_type


--------------------------------------------------------------------------------
--
-- :: Low-level custom packet sending and receiving
--
--------------------------------------------------------------------------------


data ErrFriendCustomPacket
    = ErrFriendCustomPacketOk
      -- The function returned successfully.

    | ErrFriendCustomPacketNull
      -- One of the arguments to the function was 'nullPtr' when it was not
      -- expected.

    | ErrFriendCustomPacketFriendNotFound
      -- The friend number did not designate a valid friend.

    | ErrFriendCustomPacketFriendNotConnected
      -- This client is currently not connected to the friend.

    | ErrFriendCustomPacketInvalid
      -- The first byte of data was not in the specified range for the packet
      -- type. This range is 200-254 for lossy, and 160-191 for lossless packets.

    | ErrFriendCustomPacketEmpty
      -- Attempted to send an empty packet.

    | ErrFriendCustomPacketTooLong
      -- Packet data length exceeded 'tox_max_custom_packet_size'.

    | ErrFriendCustomPacketSendq
      -- Packet queue is full.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_friend_send_lossy_packet :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
callFriendLossyPacket :: (ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool) ->
                          Tox -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
callFriendLossyPacket f tox fn d = withForeignPtr tox $ \toxPtr ->
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ f toxPtr fn dataPtr (fromIntegral dataLen)

toxFriendLossyPacket :: Tox -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendLossyPacket = callFriendLossyPacket tox_friend_send_lossy_packet

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
foreign import ccall tox_friend_send_lossless_packet :: ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
callFriendLosslessPacket :: (ToxPtr -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool) ->
                             Tox -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
callFriendLosslessPacket f tox fn d = withForeignPtr tox $ \toxPtr ->
    BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
        callErrFun $ f toxPtr fn dataPtr (fromIntegral dataLen)

toxFriendLosslessPacket :: Tox -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendLosslessPacket = callFriendLosslessPacket tox_friend_send_lossless_packet


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
foreign import ccall tox_self_get_dht_id :: ToxPtr -> CString -> IO ()

toxSelfGetDhtId :: Tox -> IO BS.ByteString
toxSelfGetDhtId tox = withForeignPtr tox $ \toxPtr ->
    let idLen = fromIntegral tox_public_key_size in
    allocaArray idLen $ \idPtr -> do
        tox_self_get_dht_id toxPtr idPtr
        BS.packCStringLen (idPtr, idLen)

data ErrGetPort
    = ErrGetPortOk
      -- The function returned successfully.

    | ErrGetPortNotBound
      -- The instance was not bound to any port.
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return the UDP port this Tox instance is bound to.
foreign import ccall tox_self_get_udp_port :: ToxPtr -> CErr ErrGetPort -> IO Word16
toxSelfGetUdpPort :: Tox -> IO (Either ErrGetPort Word16)
toxSelfGetUdpPort tox = withForeignPtr tox $ \toxPtr -> callErrFun $ tox_self_get_udp_port toxPtr

-- | Return the TCP port this Tox instance is bound to. This is only relevant if
-- the instance is acting as a TCP relay.
foreign import ccall tox_self_get_tcp_port :: ToxPtr -> CErr ErrGetPort -> IO Word16
toxSelfGetTcpPort :: Tox -> IO (Either ErrGetPort Word16)
toxSelfGetTcpPort tox = withForeignPtr tox $ \toxPtr -> callErrFun $ tox_self_get_udp_port toxPtr
