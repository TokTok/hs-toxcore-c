{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe       #-}
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

import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar (MVar, modifyMVar_)
import           Control.Exception       (bracket)
import           Control.Monad           ((>=>))
import qualified Data.ByteString         as BS
import           Data.Word               (Word16, Word32, Word64)
import           Foreign.C.String        (CString, peekCStringLen, withCString,
                                          withCStringLen)
import           Foreign.C.Types         (CInt (..), CSize (..), CTime (..))
import           Foreign.Marshal.Alloc   (alloca)
import           Foreign.Marshal.Array   (allocaArray, peekArray)
import           Foreign.Ptr             (FunPtr, Ptr, nullPtr)
import           Foreign.StablePtr       (deRefStablePtr, freeStablePtr,
                                          newStablePtr)
import           Foreign.Storable        (peek)
import           System.Posix.Types      (EpochTime)

import           Network.Tox.C.CEnum
import           Network.Tox.C.Constants
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
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Represents message types for tox_friend_send_message and group chat
-- messages.
data MessageType
  = MessageTypeNormal
    -- ^ Normal text message. Similar to PRIVMSG on IRC.
  | MessageTypeAction
    -- ^ A message describing an user action. This is similar to /me (CTCP
    -- ACTION) on IRC.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


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
foreign import ccall tox_new :: OptionsPtr -> CErr ErrNew -> IO (Tox a)

toxNew :: OptionsPtr -> IO (Either ErrNew (Tox a))
toxNew = callErrFun . tox_new

-- | Releases all resources associated with the Tox instance and disconnects
-- from the network.
--
-- After calling this function, the Tox pointer becomes invalid. No other
-- functions can be called, and the pointer value can no longer be read.
foreign import ccall tox_kill :: Tox a -> IO ()

toxKill :: Tox a -> IO ()
toxKill = tox_kill

withTox :: OptionsPtr -> (Tox a -> IO r) -> IO (Either ErrNew r)
withTox options f =
  bracket (toxNew options) (either (const $ return ()) toxKill) $ \case
    Left err -> return $ Left err
    Right ok -> Right <$> f ok


withDefaultTox :: (Tox a -> IO r) -> IO (Either ErrNew r)
withDefaultTox = withTox nullPtr


-- | Calculates the number of bytes required to store the tox instance with
-- tox_get_savedata. This function cannot fail. The result is always greater
-- than 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_get_savedata_size :: Tox a -> IO CSize

-- | Store all information associated with the tox instance to a byte array.
--
-- @param data A memory region large enough to store the tox instance data.
--   Call tox_get_savedata_size to find the number of bytes required. If this
--   parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_get_savedata :: Tox a -> CString -> IO ()

toxGetSavedata :: Tox a -> IO BS.ByteString
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
foreign import ccall tox_bootstrap :: Tox a -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ()

callBootstrapFun
  :: (Tox a -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ())
  -> Tox a -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
callBootstrapFun f tox address port pubKey =
  withCString address $ \address' ->
    BS.useAsCString pubKey $ \pubKey' ->
      callErrFun $ f tox address' (fromIntegral port) pubKey'

toxBootstrap :: Tox a -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
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
foreign import ccall tox_add_tcp_relay :: Tox a -> CString -> Word16 -> CString -> CErr ErrBootstrap -> IO ()

toxAddTcpRelay :: Tox a -> String -> Word16 -> BS.ByteString -> IO (Either ErrBootstrap ())
toxAddTcpRelay = callBootstrapFun tox_add_tcp_relay


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
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | @param connection_status Whether we are connected to the DHT.
type SelfConnectionStatusCb a = Tox a -> Connection -> a -> IO a
type CSelfConnectionStatusCb a = Tox a -> CEnum Connection -> UserData a -> IO ()
foreign import ccall "wrapper" wrapSelfConnectionStatusCb :: CSelfConnectionStatusCb a -> IO (FunPtr (CSelfConnectionStatusCb a))

callSelfConnectionStatusCb :: SelfConnectionStatusCb a -> CSelfConnectionStatusCb a
callSelfConnectionStatusCb f tox conn = deRefStablePtr >=> (`modifyMVar_` f tox (fromCEnum conn))

selfConnectionStatusCb :: SelfConnectionStatusCb a -> IO (FunPtr (CSelfConnectionStatusCb a))
selfConnectionStatusCb = wrapSelfConnectionStatusCb . callSelfConnectionStatusCb


-- | Set the callback for the `self_connection_status` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered whenever there is a change in the DHT connection
-- state. When disconnected, a client may choose to call tox_bootstrap again, to
-- reconnect to the DHT. Note that this state may frequently change for short
-- amounts of time. Clients should therefore not immediately bootstrap on
-- receiving a disconnect.
--
-- TODO: how long should a client wait before bootstrapping again?
foreign import ccall tox_callback_self_connection_status :: Tox a -> FunPtr (CSelfConnectionStatusCb a) -> IO ()

-- | Return the time in milliseconds before tox_iterate() should be called again
-- for optimal performance.
foreign import ccall tox_iteration_interval :: Tox a -> IO Word32
toxIterationInterval :: Tox a -> IO Word32
toxIterationInterval = tox_iteration_interval

-- | The main loop that needs to be run in intervals of tox_iteration_interval()
-- milliseconds.
foreign import ccall tox_iterate :: Tox a -> UserData a -> IO ()
toxIterate :: Tox a -> MVar a -> IO ()
toxIterate tox ud = bracket (newStablePtr ud) freeStablePtr (tox_iterate tox)


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
foreign import ccall tox_self_get_address :: Tox a -> CString -> IO ()

toxSelfGetAddress :: Tox a -> IO BS.ByteString
toxSelfGetAddress tox =
  let addrLen = fromIntegral tox_address_size in
  allocaArray addrLen $ \addrPtr -> do
    tox_self_get_address tox addrPtr
    BS.packCStringLen (addrPtr, addrLen)

-- | Set the 4-byte nospam part of the address.
--
-- @param nospam Any 32 bit unsigned integer.
foreign import ccall tox_self_set_nospam :: Tox a -> Word32 -> IO ()
toxSelfSetNospam :: Tox a -> Word32 -> IO ()
toxSelfSetNospam = tox_self_set_nospam

-- | Get the 4-byte nospam part of the address.
foreign import ccall tox_self_get_nospam :: Tox a -> IO Word32
toxSelfGetNospam :: Tox a -> IO Word32
toxSelfGetNospam = tox_self_get_nospam

-- | Copy the Tox Public Key (long term) from the Tox object.
--
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_public_key :: Tox a -> CString -> IO ()

toxSelfGetPublicKey :: Tox a -> IO BS.ByteString
toxSelfGetPublicKey tox =
  let pkLen = fromIntegral tox_public_key_size in
  allocaArray pkLen $ \pkPtr -> do
    tox_self_get_public_key tox pkPtr
    BS.packCStringLen (pkPtr, pkLen)

-- | Copy the Tox Secret Key from the Tox object.
--
-- @param secret_key A memory region of at least 'tox_secret_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_secret_key :: Tox a -> CString -> IO ()

toxSelfGetSecretKey :: Tox a -> IO BS.ByteString
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
foreign import ccall tox_self_set_name :: Tox a -> CString -> CSize -> CErr ErrSetInfo -> IO ()
callSelfSetNameFun :: (Tox a -> CString -> CSize -> CErr ErrSetInfo -> IO ()) ->
                      Tox a -> String -> IO (Either ErrSetInfo ())
callSelfSetNameFun f tox name =
  withCStringLen name $ \(nameStr, nameLen) ->
    callErrFun $ f tox nameStr (fromIntegral nameLen)

toxSelfSetName :: Tox a -> String -> IO (Either ErrSetInfo ())
toxSelfSetName = callSelfSetNameFun tox_self_set_name


-- | Return the length of the current nickname as passed to tox_self_set_name.
--
-- If no nickname was set before calling this function, the name is empty,
-- and this function returns 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_self_get_name_size :: Tox a -> IO CSize

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
foreign import ccall tox_self_get_name :: Tox a -> CString -> IO ()

toxSelfGetName :: Tox a -> IO String
toxSelfGetName tox = do
  nameLen <- tox_self_get_name_size tox
  allocaArray (fromIntegral nameLen) $ \namePtr -> do
    tox_self_get_name tox namePtr
    peekCStringLen (namePtr, fromIntegral nameLen)


-- | Set the client's status message.
--
-- Status message length cannot exceed 'tox_max_status_message_length'. If
-- length is 0, the status parameter is ignored (it can be 'nullPtr'), and the
-- user status is set back to empty.
foreign import ccall tox_self_set_status_message :: Tox a -> CString -> CSize -> CErr ErrSetInfo -> IO ()
callSelfSetStatusMessageFun :: (Tox a -> CString -> CSize -> CErr ErrSetInfo -> IO ()) ->
                               Tox a -> String -> IO (Either ErrSetInfo ())
callSelfSetStatusMessageFun f tox statusMsg =
  withCStringLen statusMsg $ \(statusMsgStr, statusMsgLen) ->
    callErrFun $ f tox statusMsgStr (fromIntegral statusMsgLen)

toxSelfSetStatusMessage :: Tox a -> String -> IO (Either ErrSetInfo ())
toxSelfSetStatusMessage = callSelfSetStatusMessageFun tox_self_set_status_message


-- | Return the length of the current status message as passed to tox_self_set_status_message.
--
-- If no status message was set before calling this function, the status
-- is empty, and this function returns 0.
--
-- @see threading for concurrency implications.
foreign import ccall tox_self_get_status_message_size :: Tox a -> IO CSize


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
foreign import ccall tox_self_get_status_message :: Tox a -> CString -> IO ()

toxSelfGetStatusMessage :: Tox a -> IO String
toxSelfGetStatusMessage tox = do
  statusMessageLen <- tox_self_get_status_message_size tox
  allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
    tox_self_get_status_message tox statusMessagePtr
    peekCStringLen (statusMessagePtr, fromIntegral statusMessageLen)


-- | Set the client's user status.
--
-- @param user_status One of the user statuses listed in the enumeration above.
foreign import ccall tox_self_set_status :: Tox a -> CEnum UserStatus -> IO ()
toxSelfSetStatus :: Tox a -> UserStatus -> IO ()
toxSelfSetStatus tox userStatus = tox_self_set_status tox $ toCEnum userStatus


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
foreign import ccall tox_friend_add :: Tox a -> CString -> CString -> CSize -> CErr ErrFriendAdd -> IO Word32
callFriendAddFun :: (Tox a -> CString -> CString -> CSize -> CErr ErrFriendAdd -> IO Word32) ->
                    Tox a -> BS.ByteString -> String -> IO (Either ErrFriendAdd Word32)
callFriendAddFun f tox address message =
  withCStringLen message $ \(msgStr, msgLen) ->
    BS.useAsCString address $ \addr' ->
      callErrFun $ f tox addr' msgStr (fromIntegral msgLen)

toxFriendAdd :: Tox a -> BS.ByteString -> String -> IO (Either ErrFriendAdd Word32)
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
foreign import ccall tox_friend_add_norequest :: Tox a -> CString -> CErr ErrFriendAdd -> IO Word32
callFriendAddNorequestFun :: (Tox a -> CString -> CErr ErrFriendAdd -> IO Word32) ->
                    Tox a -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
callFriendAddNorequestFun f tox address =
  BS.useAsCString address $ \addr' ->
    callErrFun $ f tox addr'

toxFriendAddNorequest :: Tox a -> BS.ByteString -> IO (Either ErrFriendAdd Word32)
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
foreign import ccall tox_friend_delete :: Tox a -> Word32 -> CErr ErrFriendDelete -> IO ()

toxFriendDelete :: Tox a -> Word32 -> IO (Either ErrFriendDelete ())
toxFriendDelete tox fn = callErrFun $ tox_friend_delete tox fn


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
foreign import ccall tox_friend_by_public_key :: Tox a -> CString -> CErr ErrFriendByPublicKey -> IO Word32
callFriendByPublicKey :: (Tox a -> CString -> CErr ErrFriendByPublicKey -> IO Word32) ->
                         Tox a -> BS.ByteString -> IO (Either ErrFriendByPublicKey Word32)
callFriendByPublicKey f tox address =
  BS.useAsCString address $ \addr' ->
    callErrFun $ f tox addr'

toxFriendByPublicKey :: Tox a -> BS.ByteString -> IO (Either ErrFriendByPublicKey Word32)
toxFriendByPublicKey = callFriendByPublicKey tox_friend_by_public_key

-- | Checks if a friend with the given friend number exists and returns true if
-- it does.
foreign import ccall tox_friend_exists :: Tox a -> Word32 -> IO Bool
toxFriendExists :: Tox a -> Word32 -> IO Bool
toxFriendExists = tox_friend_exists

-- | Return the number of friends on the friend list.
--
-- This function can be used to determine how much memory to allocate for
-- tox_self_get_friend_list.
foreign import ccall tox_self_get_friend_list_size :: Tox a -> IO CSize

-- | Copy a list of valid friend numbers into an array.
--
-- Call tox_self_get_friend_list_size to determine the number of elements to
-- allocate.
--
-- @param list A memory region with enough space to hold the friend list. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_friend_list :: Tox a -> Ptr Word32 -> IO ()

toxSelfGetFriendList :: Tox a -> IO [Word32]
toxSelfGetFriendList tox = do
  friendListSize <- tox_self_get_friend_list_size tox
  allocaArray (fromIntegral friendListSize) $ \friendListPtr -> do
    tox_self_get_friend_list tox friendListPtr
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
foreign import ccall tox_friend_get_public_key :: Tox a -> Word32 -> CString -> CErr ErrFriendGetPublicKey -> IO Bool
callFriendGetPublicKey :: (Tox a -> Word32 -> CString -> CErr ErrFriendGetPublicKey -> IO Bool) ->
                          Tox a -> Word32 -> IO (Either ErrFriendGetPublicKey BS.ByteString)
callFriendGetPublicKey f tox fn =
  let pkLen = fromIntegral tox_public_key_size in
  alloca $ \errPtr -> do
    allocaArray pkLen $ \pkPtr -> do
      _ <- f tox fn pkPtr errPtr
      err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
      str <- BS.packCStringLen (pkPtr, pkLen)
      return $ if err /= minBound
               then Left  err
               else Right str

toxFriendGetPublicKey :: Tox a -> Word32 -> IO (Either ErrFriendGetPublicKey BS.ByteString)
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
foreign import ccall tox_friend_get_last_online :: Tox a -> Word32 -> CErr ErrFriendGetLastOnline -> IO Word64
callFriendGetLastOnline :: (Tox a -> Word32 -> CErr ErrFriendGetLastOnline -> IO Word64) ->
                           Tox a -> Word32 -> IO (Either ErrFriendGetLastOnline EpochTime)
callFriendGetLastOnline f tox fn = callErrFun $ \errPtr -> (f tox fn errPtr) >>= (return . CTime . fromIntegral)

toxFriendGetLastOnline :: Tox a -> Word32 -> IO (Either ErrFriendGetLastOnline EpochTime)
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
foreign import ccall tox_friend_get_name_size :: Tox a -> Word32 -> CErr ErrFriendQuery -> IO CSize

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
foreign import ccall tox_friend_get_name :: Tox a -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool

toxFriendGetName :: Tox a -> Word32 -> IO (Either ErrFriendQuery String)
toxFriendGetName tox fn = do
  nameLenRes <- callErrFun $ tox_friend_get_name_size tox fn
  case nameLenRes of
    Left err -> return $ Left err
    Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
      nameRes <- callErrFun $ tox_friend_get_name tox fn namePtr
      case nameRes of
        Left err -> return $ Left err
        Right _ -> do
          name <- peekCStringLen (namePtr, fromIntegral nameLen)
          return $ Right name


-- | @param friend_number The friend number of the friend whose name changed.
-- @param name A byte array containing the same data as
--   tox_friend_get_name would write to its `name` parameter.
-- @param length A value equal to the return value of
--   tox_friend_get_name_size.
type FriendNameCb a = Tox a -> Word32 -> String -> a -> IO a
type CFriendNameCb a = Tox a -> Word32 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendNameCb :: CFriendNameCb a -> IO (FunPtr (CFriendNameCb a))

callFriendNameCb :: FriendNameCb a -> CFriendNameCb a
callFriendNameCb f tox fn nameStr nameLen udPtr = do
  ud <- deRefStablePtr udPtr
  name <- peekCStringLen (nameStr, fromIntegral nameLen)
  modifyMVar_ ud $ f tox fn name

friendNameCb :: FriendNameCb a -> IO (FunPtr (CFriendNameCb a))
friendNameCb = wrapFriendNameCb . callFriendNameCb


-- | Set the callback for the `friend_name` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend changes their name.
foreign import ccall tox_callback_friend_name :: Tox a -> FunPtr (CFriendNameCb a) -> IO ()


-- | @param friend_number The friend number of the friend whose status message
--   changed.
-- @param message A byte array containing the same data as
--   tox_friend_get_status_message would write to its `status_message`
--   parameter.
-- @param length A value equal to the return value of
--   tox_friend_get_status_message_size.
type FriendStatusMessageCb a = Tox a -> Word32 -> String -> a -> IO a
type CFriendStatusMessageCb a = Tox a -> Word32 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendStatusMessageCb :: CFriendStatusMessageCb a -> IO (FunPtr (CFriendStatusMessageCb a))

callFriendStatusMessageCb :: FriendStatusMessageCb a -> CFriendStatusMessageCb a
callFriendStatusMessageCb f tox fn statusMessageStr statusMessageLen udPtr = do
  ud <- deRefStablePtr udPtr
  statusMessage <- peekCStringLen (statusMessageStr, fromIntegral statusMessageLen)
  modifyMVar_ ud $ f tox fn statusMessage

friendStatusMessageCb :: FriendStatusMessageCb a -> IO (FunPtr (CFriendStatusMessageCb a))
friendStatusMessageCb = wrapFriendStatusMessageCb . callFriendStatusMessageCb


-- | Return the length of the friend's status message. If the friend number is
-- invalid, the return value is SIZE_MAX.
foreign import ccall tox_friend_get_status_message_size :: Tox a -> Word32 -> CErr ErrFriendQuery -> IO CSize

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
foreign import ccall tox_friend_get_status_message :: Tox a -> Word32 -> CString -> CErr ErrFriendQuery -> IO Bool

toxFriendGetStatusMessage :: Tox a -> Word32 -> IO (Either ErrFriendQuery String)
toxFriendGetStatusMessage tox fn = do
  statusMessageLenRes <- callErrFun $ tox_friend_get_status_message_size tox fn
  case statusMessageLenRes of
    Left err -> return $ Left err
    Right statusMessageLen -> allocaArray (fromIntegral statusMessageLen) $ \statusMessagePtr -> do
      statusMessageRes <- callErrFun $ tox_friend_get_status_message tox fn statusMessagePtr
      case statusMessageRes of
        Left err -> return $ Left err
        Right _ -> do
          statusMessage <- peekCStringLen (statusMessagePtr, fromIntegral statusMessageLen)
          return $ Right statusMessage


-- | Set the callback for the `friend_status_message` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when a friend changes their status message.
foreign import ccall tox_callback_friend_status_message :: Tox a -> FunPtr (CFriendStatusMessageCb a) -> IO ()


-- | @param friend_number The friend number of the friend whose user status
--   changed.
-- @param status The new user status.
type FriendStatusCb a = Tox a -> Word32 -> UserStatus -> a -> IO a
type CFriendStatusCb a = Tox a -> Word32 -> CEnum UserStatus -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendStatusCb :: CFriendStatusCb a -> IO (FunPtr (CFriendStatusCb a))

callFriendStatusCb :: FriendStatusCb a -> CFriendStatusCb a
callFriendStatusCb f tox fn status = deRefStablePtr >=> (`modifyMVar_` f tox fn (fromCEnum status))

friendStatusCb :: FriendStatusCb a -> IO (FunPtr (CFriendStatusCb a))
friendStatusCb = wrapFriendStatusCb . callFriendStatusCb


-- | Set the callback for the `friend_status` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend changes their user status.
foreign import ccall tox_callback_friend_status :: Tox a -> FunPtr (CFriendStatusCb a) -> IO ()


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
foreign import ccall tox_friend_get_connection_status :: Tox a -> Word32 -> CErr ErrFriendQuery -> IO (CEnum Connection)

callFriendGetConnectionStatus :: (Tox a -> Word32 -> CErr ErrFriendQuery -> IO (CEnum Connection)) ->
                                 Tox a -> Word32 -> IO (Either ErrFriendQuery Connection)
callFriendGetConnectionStatus f tox fn = callErrFun $ \errPtr -> (f tox fn errPtr) >>= (return . fromCEnum)

toxFriendGetConnectionStatus :: Tox a -> Word32 -> IO (Either ErrFriendQuery Connection)
toxFriendGetConnectionStatus = callFriendGetConnectionStatus tox_friend_get_connection_status


-- | @param friend_number The friend number of the friend whose connection
--   status changed.
-- @param connection_status The result of calling
--   tox_friend_get_connection_status on the passed friend_number.
type FriendConnectionStatusCb a = Tox a -> Word32 -> Connection -> a -> IO a
type CFriendConnectionStatusCb a = Tox a -> Word32 -> CEnum Connection -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendConnectionStatusCb :: CFriendConnectionStatusCb a -> IO (FunPtr (CFriendConnectionStatusCb a))

callFriendConnectionStatusCb :: FriendConnectionStatusCb a -> CFriendConnectionStatusCb a
callFriendConnectionStatusCb f tox fn connectionStatus = deRefStablePtr >=> (`modifyMVar_` f tox fn (fromCEnum connectionStatus))

friendConnectionStatusCb :: FriendConnectionStatusCb a -> IO (FunPtr (CFriendConnectionStatusCb a))
friendConnectionStatusCb = wrapFriendConnectionStatusCb . callFriendConnectionStatusCb


-- | Set the callback for the `friend_connection_status` event. Pass 'nullPtr'
-- to unset.
--
-- This event is triggered when a friend goes offline after having been online,
-- or when a friend goes online.
--
-- This callback is not called when adding friends. It is assumed that when
-- adding friends, their connection status is initially offline.
foreign import ccall tox_callback_friend_connection_status :: Tox a -> FunPtr (CFriendConnectionStatusCb a) -> IO ()


-- | Check whether a friend is currently typing a message.
--
-- @param friend_number The friend number for which to query the typing status.
--
-- @return true if the friend is typing.
-- @return false if the friend is not typing, or the friend number was
--   invalid. Inspect the error code to determine which case it is.
foreign import ccall tox_friend_get_typing :: Tox a -> Word32 -> CErr ErrFriendQuery -> IO Bool

callFriendGetTyping :: (Tox a -> Word32 -> CErr ErrFriendQuery -> IO Bool) ->
                       Tox a -> Word32 -> IO (Either ErrFriendQuery Bool)
callFriendGetTyping f tox fn = callErrFun $ f tox fn

toxFriendGetTyping :: Tox a -> Word32 -> IO (Either ErrFriendQuery Bool)
toxFriendGetTyping = callFriendGetTyping tox_friend_get_typing


-- | @param friend_number The friend number of the friend who started or stopped
--   typing.
-- @param is_typing The result of calling tox_friend_get_typing on the passed
--   friend_number.
type FriendTypingCb a = Tox a -> Word32 -> Bool -> a -> IO a
type CFriendTypingCb a = Tox a -> Word32 -> Bool -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendTypingCb :: CFriendTypingCb a -> IO (FunPtr (CFriendTypingCb a))

callFriendTypingCb :: FriendTypingCb a -> CFriendTypingCb a
callFriendTypingCb f tox fn typing = deRefStablePtr >=> (`modifyMVar_` f tox fn typing)

friendTypingCb :: FriendTypingCb a -> IO (FunPtr (CFriendTypingCb a))
friendTypingCb = wrapFriendTypingCb . callFriendTypingCb

-- | Set the callback for the `friend_typing` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend starts or stops typing.
foreign import ccall tox_callback_friend_typing :: Tox a -> FunPtr (CFriendTypingCb a) -> IO ()


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
foreign import ccall tox_self_set_typing :: Tox a -> Word32 -> Bool -> CErr ErrSetTyping -> IO Bool
callSelfSetTyping :: (Tox a -> Word32 -> Bool -> CErr ErrSetTyping -> IO Bool) ->
                     Tox a -> Word32 -> Bool -> IO (Either ErrSetTyping Bool)
callSelfSetTyping f tox fn typing = callErrFun $ f tox fn typing

toxSelfSetTyping :: Tox a -> Word32 -> Bool -> IO (Either ErrSetTyping Bool)
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
foreign import ccall tox_friend_send_message :: Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrFriendSendMessage -> IO Word32
callFriendSendMessage :: (Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrFriendSendMessage -> IO Word32) ->
                         Tox a -> Word32 -> MessageType -> String -> IO (Either ErrFriendSendMessage Word32)
callFriendSendMessage f tox fn messageType message =
  withCStringLen message $ \(msgStr, msgLen) ->
    callErrFun $ f tox fn (toCEnum messageType) msgStr (fromIntegral msgLen)

toxFriendSendMessage :: Tox a -> Word32 -> MessageType -> String -> IO (Either ErrFriendSendMessage Word32)
toxFriendSendMessage = callFriendSendMessage tox_friend_send_message


-- | @param friend_number The friend number of the friend who received the
--   message.
-- @param message_id The message ID as returned from tox_friend_send_message
--   corresponding to the message sent.
type FriendReadReceiptCb a = Tox a -> Word32 -> Word32 -> a -> IO a
type CFriendReadReceiptCb a = Tox a -> Word32 -> Word32 -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendReadReceiptCb :: CFriendReadReceiptCb a -> IO (FunPtr (CFriendReadReceiptCb a))

callFriendReadReceiptCb :: FriendReadReceiptCb a -> CFriendReadReceiptCb a
callFriendReadReceiptCb f tox fn msgId = deRefStablePtr >=> (`modifyMVar_` f tox fn msgId)

friendReadReceiptCb :: FriendReadReceiptCb a -> IO (FunPtr (CFriendReadReceiptCb a))
friendReadReceiptCb = wrapFriendReadReceiptCb . callFriendReadReceiptCb


-- | Set the callback for the `friend_read_receipt` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when the friend receives the message sent with
-- tox_friend_send_message with the corresponding message ID.
foreign import ccall tox_callback_friend_read_receipt :: Tox a -> FunPtr (CFriendReadReceiptCb a) -> IO ()


--------------------------------------------------------------------------------
--
-- :: Receiving private messages and friend requests
--
--------------------------------------------------------------------------------


-- | @param public_key The Public Key of the user who sent the friend request.
-- @param time_delta A delta in seconds between when the message was composed
--   and when it is being transmitted. For messages that are sent immediately,
--   it will be 0. If a message was written and couldn't be sent immediately
--   (due to a connection failure, for example), the time_delta is an
--   approximation of when it was composed.
-- @param message The message they sent along with the request.
-- @param length The size of the message byte array.
type FriendRequestCb a = Tox a -> BS.ByteString -> String -> a -> IO a
type CFriendRequestCb a = Tox a -> CString -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendRequestCb :: CFriendRequestCb a -> IO (FunPtr (CFriendRequestCb a))

callFriendRequestCb :: FriendRequestCb a -> CFriendRequestCb a
callFriendRequestCb f tox addrPtr nameStr nameLen udPtr = do
  let addrLen = fromIntegral tox_address_size
  ud <- deRefStablePtr udPtr
  addr <- BS.packCStringLen (addrPtr, addrLen)
  name <- peekCStringLen (nameStr, fromIntegral nameLen)
  modifyMVar_ ud $ f tox addr name

friendRequestCb :: FriendRequestCb a -> IO (FunPtr (CFriendRequestCb a))
friendRequestCb = wrapFriendRequestCb . callFriendRequestCb


-- | Set the callback for the `friend_request` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend request is received.
foreign import ccall tox_callback_friend_request :: Tox a -> FunPtr (CFriendRequestCb a) -> IO ()

-- | @param friend_number The friend number of the friend who sent the message.
-- @param time_delta Time between composition and sending.
-- @param message The message data they sent.
-- @param length The size of the message byte array.
--
-- @see friend_request for more information on time_delta.
type FriendMessageCb a = Tox a -> Word32 -> MessageType -> String -> a -> IO a
type CFriendMessageCb a = Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendMessageCb :: CFriendMessageCb a -> IO (FunPtr (CFriendMessageCb a))

callFriendMessageCb :: FriendMessageCb a -> CFriendMessageCb a
callFriendMessageCb f tox fn msgType msgStr msgLen udPtr = do
  ud <- deRefStablePtr udPtr
  msg <- peekCStringLen (msgStr, fromIntegral msgLen)
  modifyMVar_ ud $ f tox fn (fromCEnum msgType) msg

friendMessageCb :: FriendMessageCb a -> IO (FunPtr (CFriendMessageCb a))
friendMessageCb = wrapFriendMessageCb . callFriendMessageCb


-- | Set the callback for the `friend_message` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a message from a friend is received.
foreign import ccall tox_callback_friend_message :: Tox a -> FunPtr (CFriendMessageCb a) -> IO ()


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
toxHash d = do
  let hashLen = fromIntegral tox_hash_length
  allocaArray hashLen $ \hashPtr -> do
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
foreign import ccall tox_file_control :: Tox a -> Word32 -> Word32 -> CEnum FileControl -> CErr ErrFileControl -> IO Bool

callFileControl :: (Tox a -> Word32 -> Word32 -> CEnum FileControl -> CErr ErrFileControl -> IO Bool) ->
                   Tox a -> Word32 -> Word32 -> FileControl -> IO (Either ErrFileControl Bool)
callFileControl f tox fn fileNum control = callErrFun $ f tox fn fileNum (toCEnum control)

toxFileControl :: Tox a -> Word32 -> Word32 -> FileControl -> IO (Either ErrFileControl Bool)
toxFileControl = callFileControl tox_file_control

-- | When receiving FileControlCancel, the client should release the
-- resources associated with the file number and consider the transfer failed.
--
-- @param friend_number The friend number of the friend who is sending the file.
-- @param file_number The friend-specific file number the data received is
--   associated with.
-- @param control The file control command received.
type FileRecvControlCb a = Tox a -> Word32 -> Word32 -> FileControl -> a -> IO a
type CFileRecvControlCb a = Tox a -> Word32 -> Word32 -> CEnum FileControl -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFileRecvControlCb :: CFileRecvControlCb a -> IO (FunPtr (CFileRecvControlCb a))

callFileRecvControlCb :: FileRecvControlCb a -> CFileRecvControlCb a
callFileRecvControlCb f tox fn fileNum ctrlCmd = deRefStablePtr >=> (`modifyMVar_` f tox fn fileNum (fromCEnum ctrlCmd))

fileRecvControlCb :: FileRecvControlCb a -> IO (FunPtr (CFileRecvControlCb a))
fileRecvControlCb = wrapFileRecvControlCb . callFileRecvControlCb


-- | Set the callback for the `file_recv_control` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when a file control command is received from a
-- friend.
foreign import ccall tox_callback_file_recv_control :: Tox a -> FunPtr (CFileRecvControlCb a) -> IO ()

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
foreign import ccall tox_file_seek :: Tox a -> Word32 -> Word32 -> Word64 -> CErr ErrFileSeek -> IO Bool

callFileSeek :: (Tox a -> Word32 -> Word32 -> Word64 -> CErr ErrFileSeek -> IO Bool) ->
                   Tox a -> Word32 -> Word32 -> Word64 -> IO (Either ErrFileSeek Bool)
callFileSeek f tox fn fileNum pos = callErrFun $ f tox fn fileNum pos

toxFileSeek :: Tox a -> Word32 -> Word32 -> Word64 -> IO (Either ErrFileSeek Bool)
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
foreign import ccall tox_file_get_file_id :: Tox a -> Word32 -> Word32 -> CString -> CErr ErrFileGet -> IO Bool

callFileGetFileId :: (Tox a -> Word32 -> Word32 -> CString -> CErr ErrFileGet -> IO Bool) ->
                     Tox a -> Word32 -> Word32 -> IO (Either ErrFileGet BS.ByteString)
callFileGetFileId f tox fn fileNum =
  let fileIdLen = fromIntegral tox_file_id_length in
  alloca $ \errPtr -> do
    allocaArray fileIdLen $ \fileIdPtr -> do
      _ <- f tox fn fileNum fileIdPtr errPtr
      err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
      fileId <- BS.packCStringLen (fileIdPtr, fileIdLen)
      return $ if err /= minBound
               then Left  err
               else Right fileId

toxFileGetFileId :: Tox a -> Word32 -> Word32 -> IO (Either ErrFileGet BS.ByteString)
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
foreign import ccall tox_file_send :: Tox a -> Word32 -> CEnum FileKind -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32
callFileSend :: (Tox a -> Word32 -> CEnum FileKind -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32) ->
                Tox a -> Word32 -> FileKind -> Word64 -> String -> IO (Either ErrFileSend Word32)
callFileSend f tox fn fileKind fileSize fileName =
  withCStringLen fileName $ \(fileNamePtr, fileNameLen) ->
    callErrFun $ f tox fn (toCEnum fileKind) fileSize nullPtr fileNamePtr (fromIntegral fileNameLen)

toxFileSend :: Tox a -> Word32 -> FileKind -> Word64 -> String -> IO (Either ErrFileSend Word32)
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
foreign import ccall tox_file_send_chunk :: Tox a -> Word32 -> Word32 -> Word64 -> CString -> CSize -> CErr ErrFileSendChunk -> IO Bool
callFileSendChunk :: (Tox a -> Word32 -> Word32 -> Word64 -> CString -> CSize -> CErr ErrFileSendChunk -> IO Bool) ->
                Tox a -> Word32 -> Word32 -> Word64 -> BS.ByteString ->  IO (Either ErrFileSendChunk Bool)
callFileSendChunk f tox fn fileNum pos d =
  BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
    callErrFun $ f tox fn fileNum pos dataPtr (fromIntegral dataLen)

toxFileSendChunk :: Tox a -> Word32 -> Word32 -> Word64 -> BS.ByteString -> IO (Either ErrFileSendChunk Bool)
toxFileSendChunk = callFileSendChunk tox_file_send_chunk

-- | If the length parameter is 0, the file transfer is finished, and the
-- client's resources associated with the file number should be released. After
-- a call with zero length, the file number can be reused for future file
-- transfers.
--
-- If the requested position is not equal to the client's idea of the current
-- file or stream position, it will need to seek. In case of read-once streams,
-- the client should keep the last read chunk so that a seek back can be
-- supported. A seek-back only ever needs to read from the last requested
-- chunk.  This happens when a chunk was requested, but the send failed. A
-- seek-back request can occur an arbitrary number of times for any given
-- chunk.
--
-- In response to receiving this callback, the client should call the function
-- `tox_file_send_chunk` with the requested chunk. If the number of bytes sent
-- through that function is zero, the file transfer is assumed complete. A
-- client must send the full length of data requested with this callback.
--
-- @param friend_number The friend number of the receiving friend for this file.
-- @param file_number The file transfer identifier returned by tox_file_send.
-- @param position The file or stream position from which to continue reading.
-- @param length The number of bytes requested for the current chunk.
type FileChunkRequestCb a = Tox a -> Word32 -> Word32 -> Word64 -> CSize -> a -> IO a
type CFileChunkRequestCb a = Tox a -> Word32 -> Word32 -> Word64 -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFileChunkRequestCb :: CFileChunkRequestCb a -> IO (FunPtr (CFileChunkRequestCb a))

callFileChunkRequestCb :: FileChunkRequestCb a -> CFileChunkRequestCb a
callFileChunkRequestCb f tox fn fileNum pos len = deRefStablePtr >=> (`modifyMVar_` f tox fn fileNum pos len)

fileChunkRequestCb :: FileChunkRequestCb a -> IO (FunPtr (CFileChunkRequestCb a))
fileChunkRequestCb = wrapFileChunkRequestCb . callFileChunkRequestCb

-- | Set the callback for the `file_chunk_request` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when Core is ready to send more file data.
foreign import ccall tox_callback_file_chunk_request :: Tox a -> FunPtr (CFileChunkRequestCb a) -> IO ()


--------------------------------------------------------------------------------
--
-- :: File transmission: receiving
--
--------------------------------------------------------------------------------


-- | The client should acquire resources to be associated with the file
-- transfer.  Incoming file transfers start in the Paused state. After this
-- callback returns, a transfer can be rejected by sending a FileControlCancel
-- control command before any other control commands. It can be accepted by
-- sending FileControlResume.
--
-- @param friend_number The friend number of the friend who is sending the file
--   transfer request.
-- @param file_number The friend-specific file number the data received is
--   associated with.
-- @param kind The meaning of the file to be sent.
-- @param file_size Size in bytes of the file the client wants to send,
--   UINT64_MAX if unknown or streaming.
-- @param filename Name of the file. Does not need to be the actual name. This
--   name will be sent along with the file send request.
-- @param filename_length Size in bytes of the filename.
type FileRecvCb a = Tox a -> Word32 -> Word32 -> FileKind -> Word64 -> String -> a -> IO a
type CFileRecvCb a = Tox a -> Word32 -> Word32 -> CEnum FileKind -> Word64 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFileRecvCb :: CFileRecvCb a -> IO (FunPtr (CFileRecvCb a))

callFileRecvCb :: FileRecvCb a -> CFileRecvCb a
callFileRecvCb f tox fn fileNum fileKind fileSize fileNameStr fileNameLen udPtr = do
  ud <- deRefStablePtr udPtr
  fileName <- peekCStringLen (fileNameStr, fromIntegral fileNameLen)
  modifyMVar_ ud $ f tox fn fileNum (fromCEnum fileKind) fileSize fileName

fileRecvCb :: FileRecvCb a -> IO (FunPtr (CFileRecvCb a))
fileRecvCb = wrapFileRecvCb . callFileRecvCb


-- | Set the callback for the `file_recv` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a file transfer request is received.
foreign import ccall tox_callback_file_recv :: Tox a -> FunPtr (CFileRecvCb a) -> IO ()

-- | When length is 0, the transfer is finished and the client should release
-- the resources it acquired for the transfer. After a call with length = 0,
-- the file number can be reused for new file transfers.
--
-- If position is equal to file_size (received in the file_receive callback)
-- when the transfer finishes, the file was received completely. Otherwise, if
-- file_size was UINT64_MAX, streaming ended successfully when length is 0.
--
-- @param friend_number The friend number of the friend who is sending the file.
-- @param file_number The friend-specific file number the data received is
--   associated with.
-- @param position The file position of the first byte in data.
-- @param data A byte array containing the received chunk.
-- @param length The length of the received chunk.
type FileRecvChunkCb a = Tox a -> Word32 -> Word32 -> Word64 -> BS.ByteString -> a -> IO a
type CFileRecvChunkCb a = Tox a -> Word32 -> Word32 -> Word64 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFileRecvChunkCb :: CFileRecvChunkCb a -> IO (FunPtr (CFileRecvChunkCb a))

callFileRecvChunkCb :: FileRecvChunkCb a -> CFileRecvChunkCb a
callFileRecvChunkCb f tox fn fileNum pos dataPtr dataLen udPtr = do
  ud <- deRefStablePtr udPtr
  d <- BS.packCStringLen (dataPtr, fromIntegral dataLen)
  modifyMVar_ ud $ f tox fn fileNum pos d

fileRecvChunkCb :: FileRecvChunkCb a -> IO (FunPtr (CFileRecvChunkCb a))
fileRecvChunkCb = wrapFileRecvChunkCb . callFileRecvChunkCb


-- | Set the callback for the `file_recv_chunk` event. Pass 'nullPtr' to unset.
--
-- This event is first triggered when a file transfer request is received, and
-- subsequently when a chunk of file data for an accepted request was received.
foreign import ccall tox_callback_file_recv_chunk :: Tox a -> FunPtr (CFileRecvChunkCb a) -> IO ()


--------------------------------------------------------------------------------
--
-- :: Conference management
--
--------------------------------------------------------------------------------


-- | Conference types for the conference_invite event.
data ConferenceType
  = ConferenceTypeText
    -- Text-only conferences that must be accepted with the tox_conference_join function.

  | ConferenceTypeAv
    -- Video conference. The function to accept these is in toxav.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | The invitation will remain valid until the inviting friend goes offline
-- or exits the conference.
--
-- @param friend_number The friend who invited us.
-- @param type The conference type (text only or audio/video).
-- @param cookie A piece of data of variable length required to join the
--   conference.
-- @param length The length of the cookie.
type ConferenceInviteCb a = Tox a -> Word32 -> ConferenceType -> BS.ByteString -> a -> IO a
type CConferenceInviteCb a = Tox a -> Word32 -> CEnum ConferenceType -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapConferenceInviteCb :: CConferenceInviteCb a -> IO (FunPtr (CConferenceInviteCb a))

callConferenceInviteCb :: ConferenceInviteCb a -> CConferenceInviteCb a
callConferenceInviteCb f tox fn confType cookiePtr cookieLen udPtr = do
  ud <- deRefStablePtr udPtr
  cookie <- BS.packCStringLen (cookiePtr, fromIntegral cookieLen)
  modifyMVar_ ud $ f tox fn (fromCEnum confType) cookie

conferenceInviteCb :: ConferenceInviteCb a -> IO (FunPtr (CConferenceInviteCb a))
conferenceInviteCb = wrapConferenceInviteCb . callConferenceInviteCb


-- | Set the callback for the `conference_invite` event. Pass NULL to unset.
--
-- This event is triggered when the client is invited to join a conference.
foreign import ccall tox_callback_conference_invite :: Tox a -> FunPtr (CConferenceInviteCb a) -> IO ()


-- | @param conference_number The conference number of the conference the message is intended for.
-- @param peer_number The ID of the peer who sent the message.
-- @param type The type of message (normal, action, ...).
-- @param message The message data.
-- @param length The length of the message.
type ConferenceMessageCb a = Tox a -> Word32 -> Word32 -> MessageType -> String -> a -> IO a
type CConferenceMessageCb a = Tox a -> Word32 -> Word32 -> CEnum MessageType -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapConferenceMessageCb :: CConferenceMessageCb a -> IO (FunPtr (CConferenceMessageCb a))

callConferenceMessageCb :: ConferenceMessageCb a -> CConferenceMessageCb a
callConferenceMessageCb f tox gn pn msgType msgStr msgLen udPtr = do
  ud <- deRefStablePtr udPtr
  msg <- peekCStringLen (msgStr, fromIntegral msgLen)
  modifyMVar_ ud $ f tox gn pn (fromCEnum msgType) msg

conferenceMessageCb :: ConferenceMessageCb a -> IO (FunPtr (CConferenceMessageCb a))
conferenceMessageCb = wrapConferenceMessageCb . callConferenceMessageCb


-- | Set the callback for the `conference_message` event. Pass NULL to unset.
--
-- This event is triggered when the client receives a conference message.
foreign import ccall tox_callback_conference_message :: Tox a -> FunPtr (CConferenceMessageCb a) -> IO ()


-- | @param conference_number The conference number of the conference the title change is intended for.
-- @param peer_number The ID of the peer who changed the title.
-- @param title The title data.
-- @param length The title length.
type ConferenceTitleCb a = Tox a -> Word32 -> Word32 -> String -> a -> IO a
type CConferenceTitleCb a = Tox a -> Word32 -> Word32 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapConferenceTitleCb :: CConferenceTitleCb a -> IO (FunPtr (CConferenceTitleCb a))

callConferenceTitleCb :: ConferenceTitleCb a -> CConferenceTitleCb a
callConferenceTitleCb f tox gn pn titleStr titleLen udPtr = do
  ud <- deRefStablePtr udPtr
  title <- peekCStringLen (titleStr, fromIntegral titleLen)
  modifyMVar_ ud $ f tox gn pn title

conferenceTitleCb :: ConferenceTitleCb a -> IO (FunPtr (CConferenceTitleCb a))
conferenceTitleCb = wrapConferenceTitleCb . callConferenceTitleCb


-- | Set the callback for the `conference_title` event. Pass NULL to unset.
--
-- This event is triggered when a peer changes the conference title.
--
-- If peer_number == UINT32_MAX, then author is unknown (e.g. initial joining the conference).
foreign import ccall tox_callback_conference_title :: Tox a -> FunPtr (CConferenceTitleCb a) -> IO ()


-- | Peer list state change types.
data ConferenceStateChange
  = ConferenceStateChangeListChanged
    -- A peer has joined or exited the conference.

  | ConferenceStateChangePeerNameChange
    -- A peer has changed their name.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | @param conference_number The conference number of the conference the title change is intended for.
-- @param peer_number The ID of the peer who changed the title.
-- @param change The type of change (one of TOX_CONFERENCE_STATE_CHANGE).
type ConferenceNamelistChangeCb a = Tox a -> Word32 -> Word32 -> ConferenceStateChange -> a -> IO a
type CConferenceNamelistChangeCb a = Tox a -> Word32 -> Word32 -> CEnum ConferenceStateChange -> UserData a -> IO ()
foreign import ccall "wrapper" wrapConferenceNamelistChangeCb :: CConferenceNamelistChangeCb a -> IO (FunPtr (CConferenceNamelistChangeCb a))

callConferenceNamelistChangeCb :: ConferenceNamelistChangeCb a -> CConferenceNamelistChangeCb a
callConferenceNamelistChangeCb f tox gn pn changeType = deRefStablePtr >=> (`modifyMVar_` f tox gn pn (fromCEnum changeType))

conferenceNamelistChangeCb :: ConferenceNamelistChangeCb a -> IO (FunPtr (CConferenceNamelistChangeCb a))
conferenceNamelistChangeCb = wrapConferenceNamelistChangeCb . callConferenceNamelistChangeCb


-- | Set the callback for the `conference_namelist_change` event. Pass NULL to unset.
--
-- This event is triggered when the peer list changes (name change, peer join, peer exit).
foreign import ccall tox_callback_conference_namelist_change :: Tox a -> FunPtr (CConferenceNamelistChangeCb a) -> IO ()


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
foreign import ccall tox_conference_new :: Tox a -> CErr ErrConferenceNew -> IO Word32
callConferenceNew :: (Tox a -> CErr ErrConferenceNew -> IO Word32) ->
                     Tox a -> IO (Either ErrConferenceNew Word32)
callConferenceNew f tox = callErrFun $ f tox

toxConferenceNew :: Tox a -> IO (Either ErrConferenceNew Word32)
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
foreign import ccall tox_conference_delete :: Tox a -> Word32 -> CErr ErrConferenceDelete -> IO Bool
callConferenceDelete :: (Tox a -> Word32 -> CErr ErrConferenceDelete -> IO Bool) ->
                        Tox a -> Word32 -> IO (Either ErrConferenceDelete Bool)
callConferenceDelete f tox gn = callErrFun $ f tox gn

toxConferenceDelete :: Tox a -> Word32 -> IO (Either ErrConferenceDelete Bool)
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
foreign import ccall tox_conference_peer_count :: Tox a -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32
callConferencePeerCount :: (Tox a -> Word32 -> CErr ErrConferencePeerQuery -> IO Word32) ->
                           Tox a -> Word32 -> IO (Either ErrConferencePeerQuery Word32)
callConferencePeerCount f tox gn = callErrFun $ f tox gn

toxConferencePeerCount :: Tox a -> Word32 -> IO (Either ErrConferencePeerQuery Word32)
toxConferencePeerCount = callConferencePeerCount tox_conference_peer_count


-- | Return the length of the peer's name. Return value is unspecified on failure.
foreign import ccall tox_conference_peer_get_name_size :: Tox a -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO CSize


-- | Copy the name of peer_number who is in conference_number to name.
-- name must be at least TOX_MAX_NAME_LENGTH long.
--
-- @return true on success.
foreign import ccall tox_conference_peer_get_name :: Tox a -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool

toxConferencePeerGetName :: Tox a -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery String)
toxConferencePeerGetName tox gn pn = do
  nameLenRes <- callErrFun $ tox_conference_peer_get_name_size tox gn pn
  case nameLenRes of
    Left err -> return $ Left err
    Right nameLen -> allocaArray (fromIntegral nameLen) $ \namePtr -> do
      nameRes <- callErrFun $ tox_conference_peer_get_name tox gn pn namePtr
      case nameRes of
        Left err -> return $ Left err
        Right _ -> do
          name <- peekCStringLen (namePtr, fromIntegral nameLen)
          return $ Right name


-- | Copy the public key of peer_number who is in conference_number to public_key.
-- public_key must be TOX_PUBLIC_KEY_SIZE long.
--
-- @return true on success.
foreign import ccall tox_conference_peer_get_public_key :: Tox a -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool
callConferencePeerGetPublicKey :: (Tox a -> Word32 -> Word32 -> CString -> CErr ErrConferencePeerQuery -> IO Bool) ->
                          Tox a -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
callConferencePeerGetPublicKey f tox gn pn =
  let pkLen = fromIntegral tox_public_key_size in
  alloca $ \errPtr -> do
    allocaArray pkLen $ \pkPtr -> do
      _ <- f tox gn pn pkPtr errPtr
      err <- toEnum . fromIntegral . unCEnum <$> peek errPtr
      str <- BS.packCStringLen (pkPtr, pkLen)
      return $ if err /= minBound
               then Left  err
               else Right str

toxConferencePeerGetPublicKey :: Tox a -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery BS.ByteString)
toxConferencePeerGetPublicKey = callConferencePeerGetPublicKey tox_conference_peer_get_public_key


-- | Return true if passed peer_number corresponds to our own.
foreign import ccall tox_conference_peer_number_is_ours :: Tox a -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Bool
callConferencePeerNumberIsOurs :: (Tox a -> Word32 -> Word32 -> CErr ErrConferencePeerQuery -> IO Bool) ->
                                  Tox a -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery Bool)
callConferencePeerNumberIsOurs f tox gn pn = callErrFun $ f tox gn pn

toxConferencePeerNumberIsOurs :: Tox a -> Word32 -> Word32 -> IO (Either ErrConferencePeerQuery Bool)
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
foreign import ccall tox_conference_invite :: Tox a -> Word32 -> Word32 -> CErr ErrConferenceInvite -> IO Bool
callConferenceInvite :: (Tox a -> Word32 -> Word32 -> CErr ErrConferenceInvite -> IO Bool) ->
                        Tox a -> Word32 -> Word32 -> IO (Either ErrConferenceInvite Bool)
callConferenceInvite f tox fn gn = callErrFun $ f tox fn gn

toxConferenceInvite :: Tox a -> Word32 -> Word32 -> IO (Either ErrConferenceInvite Bool)
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
foreign import ccall tox_conference_join :: Tox a -> Word32 -> CString -> CSize -> CErr ErrConferenceJoin -> IO Word32
callConferenceJoin :: (Tox a -> Word32 -> CString -> CSize -> CErr ErrConferenceJoin -> IO Word32) ->
                        Tox a -> Word32 -> BS.ByteString -> IO (Either ErrConferenceJoin Word32)
callConferenceJoin f tox fn cookie = do
  BS.useAsCStringLen cookie $ \(cookiePtr, cookieLen) ->
    callErrFun $ f tox fn cookiePtr (fromIntegral cookieLen)

toxConferenceJoin :: Tox a -> Word32 -> BS.ByteString -> IO (Either ErrConferenceJoin Word32)
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
foreign import ccall tox_conference_send_message :: Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrConferenceSendMessage -> IO Bool
callConferenceSendMessage :: (Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> CErr ErrConferenceSendMessage -> IO Bool) ->
                        Tox a -> Word32 -> MessageType -> String -> IO (Either ErrConferenceSendMessage Bool)
callConferenceSendMessage f tox gn messageType message = do
  withCStringLen message $ \(msgPtr, msgLen) ->
    callErrFun $ f tox gn (toCEnum messageType) msgPtr (fromIntegral msgLen)

toxConferenceSendMessage :: Tox a -> Word32 -> MessageType -> String -> IO (Either ErrConferenceSendMessage Bool)
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
foreign import ccall tox_conference_get_title_size :: Tox a -> Word32 -> CErr ErrConferenceTitle -> IO CSize


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
foreign import ccall tox_conference_get_title :: Tox a -> Word32 -> CString -> CErr ErrConferenceTitle -> IO Bool

toxConferenceGetTitle :: Tox a -> Word32 -> IO (Either ErrConferenceTitle String)
toxConferenceGetTitle tox gn = do
  titleLenRes <- callErrFun $ tox_conference_get_title_size tox gn
  case titleLenRes of
    Left err -> return $ Left err
    Right titleLen -> allocaArray (fromIntegral titleLen) $ \titlePtr -> do
      titleRes <- callErrFun $ tox_conference_get_title tox gn titlePtr
      case titleRes of
        Left err -> return $ Left err
        Right _ -> do
          title <- peekCStringLen (titlePtr, fromIntegral titleLen)
          return $ Right title

-- | Set the conference title and broadcast it to the rest of the conference.
--
-- Title length cannot be longer than TOX_MAX_NAME_LENGTH.
--
-- @return true on success.
foreign import ccall tox_conference_set_title :: Tox a -> Word32 -> CString -> CSize -> CErr ErrConferenceTitle -> IO Bool
callConferenceSetTitle :: (Tox a -> Word32 -> CString -> CSize -> CErr ErrConferenceTitle -> IO Bool) ->
                        Tox a -> Word32 -> String -> IO (Either ErrConferenceTitle Bool)
callConferenceSetTitle f tox gn title = do
  withCStringLen title $ \(titlePtr, titleLen) ->
    callErrFun $ f tox gn titlePtr (fromIntegral titleLen)

toxConferenceSetTitle :: Tox a -> Word32 -> String -> IO (Either ErrConferenceTitle Bool)
toxConferenceSetTitle = callConferenceSetTitle tox_conference_set_title


-- | Return the number of conferences in the Tox instance.
-- This should be used to determine how much memory to allocate for `tox_conference_get_chatlist`.
foreign import ccall tox_conference_get_chatlist_size :: Tox a -> IO CSize


-- | Copy a list of valid conference IDs into the array chatlist. Determine how much space
-- to allocate for the array with the `tox_conference_get_chatlist_size` function.
foreign import ccall tox_conference_get_chatlist :: Tox a -> Ptr Word32 -> IO ()

toxConferenceGetChatlist :: Tox a -> IO [Word32]
toxConferenceGetChatlist tox = do
  chatListSize <- tox_conference_get_chatlist_size tox
  allocaArray (fromIntegral chatListSize) $ \chatListPtr -> do
    tox_conference_get_chatlist tox chatListPtr
    peekArray (fromIntegral chatListSize) chatListPtr

-- | Returns the type of conference (TOX_CONFERENCE_TYPE) that conference_number is. Return value is
-- unspecified on failure.
data ErrConferenceGetType
  = ErrConferenceGetTypeOk
    -- The function returned successfully.

  | ErrConferenceGetTypeConferenceNotFound
    -- The conference number passed did not designate a valid conference.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

foreign import ccall tox_conference_get_type :: Tox a -> Word32 -> CErr ErrConferenceGetType -> IO (CEnum ConferenceType)
callConferenceGetType :: (Tox a -> Word32 -> CErr ErrConferenceGetType -> IO (CEnum ConferenceType)) ->
                         Tox a -> Word32 -> IO (Either ErrConferenceGetType ConferenceType)
callConferenceGetType f tox gn = callErrFun $ \errPtr -> (f tox gn errPtr) >>= (return . fromCEnum)

toxConferenceGetType :: Tox a -> Word32 -> IO (Either ErrConferenceGetType ConferenceType)
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
foreign import ccall tox_friend_send_lossy_packet :: Tox a -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
callFriendLossyPacket :: (Tox a -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool) ->
                         Tox a -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
callFriendLossyPacket f tox fn d =
  BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
    callErrFun $ f tox fn dataPtr (fromIntegral dataLen)

toxFriendLossyPacket :: Tox a -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
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
foreign import ccall tox_friend_send_lossless_packet :: Tox a -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool
callFriendLoselessPacket :: (Tox a -> Word32 -> CString -> CSize -> CErr ErrFriendCustomPacket -> IO Bool) ->
                         Tox a -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
callFriendLoselessPacket f tox fn d =
  BS.useAsCStringLen d $ \(dataPtr, dataLen) ->
    callErrFun $ f tox fn dataPtr (fromIntegral dataLen)

toxFriendLoselessPacket :: Tox a -> Word32 -> BS.ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendLoselessPacket = callFriendLoselessPacket tox_friend_send_lossless_packet

-- | @param friend_number The friend number of the friend who sent a lossy
-- packet.
-- @param data A byte array containing the received packet data.
-- @param length The length of the packet data byte array.
type FriendLossyPacketCb a = Tox a -> Word32 -> BS.ByteString -> a -> IO a
type CFriendLossyPacketCb a = Tox a -> Word32 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendLossyPacketCb :: CFriendLossyPacketCb a -> IO (FunPtr (CFriendLossyPacketCb a))

callFriendLossyPacketCb :: FriendLossyPacketCb a -> CFriendLossyPacketCb a
callFriendLossyPacketCb f tox fn dataPtr dataLen udPtr = do
  ud <- deRefStablePtr udPtr
  d <- BS.packCStringLen (dataPtr, fromIntegral dataLen)
  modifyMVar_ ud $ f tox fn d

friendLossyPacketCb :: FriendLossyPacketCb a -> IO (FunPtr (CFriendLossyPacketCb a))
friendLossyPacketCb = wrapFriendLossyPacketCb . callFriendLossyPacketCb


-- | Set the callback for the `friend_lossy_packet` event. Pass 'nullPtr' to
-- unset.
--
foreign import ccall tox_callback_friend_lossy_packet :: Tox a -> FunPtr (CFriendLossyPacketCb a) -> IO ()

-- | @param friend_number The friend number of the friend who sent the packet.
-- @param data A byte array containing the received packet data.
-- @param length The length of the packet data byte array.
type FriendLosslessPacketCb a = Tox a -> Word32 -> BS.ByteString -> a -> IO a
type CFriendLosslessPacketCb a = Tox a -> Word32 -> CString -> CSize -> UserData a -> IO ()
foreign import ccall "wrapper" wrapFriendLosslessPacketCb :: CFriendLosslessPacketCb a -> IO (FunPtr (CFriendLosslessPacketCb a))

callFriendLosslessPacketCb :: FriendLosslessPacketCb a -> CFriendLosslessPacketCb a
callFriendLosslessPacketCb f tox fn dataPtr dataLen udPtr = do
  ud <- deRefStablePtr udPtr
  d <- BS.packCStringLen (dataPtr, fromIntegral dataLen)
  modifyMVar_ ud $ f tox fn d

friendLosslessPacketCb :: FriendLosslessPacketCb a -> IO (FunPtr (CFriendLosslessPacketCb a))
friendLosslessPacketCb = wrapFriendLosslessPacketCb . callFriendLosslessPacketCb


-- | Set the callback for the `friend_lossless_packet` event. Pass 'nullPtr' to
-- unset.
--
foreign import ccall tox_callback_friend_lossless_packet :: Tox a -> FunPtr (CFriendLosslessPacketCb a) -> IO ()


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
foreign import ccall tox_self_get_dht_id :: Tox a -> CString -> IO ()

toxSelfGetDhtId :: Tox a -> IO BS.ByteString
toxSelfGetDhtId tox =
  let idLen = fromIntegral tox_public_key_size in
  allocaArray idLen $ \idPtr -> do
    tox_self_get_dht_id tox idPtr
    BS.packCStringLen (idPtr, idLen)

data ErrGetPort
  = ErrGetPortOk
    -- The function returned successfully.

  | ErrGetPortNotBound
    -- The instance was not bound to any port.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return the UDP port this Tox instance is bound to.
foreign import ccall tox_self_get_udp_port :: Tox a -> CErr ErrGetPort -> IO Word16
callSelfGetUdpPort :: (Tox a -> CErr ErrGetPort -> IO Word16) ->
                        Tox a -> IO (Either ErrGetPort Word16)
callSelfGetUdpPort f tox = callErrFun $ f tox

toxSelfGetUdpPort :: Tox a -> IO (Either ErrGetPort Word16)
toxSelfGetUdpPort = callSelfGetUdpPort tox_self_get_udp_port

-- | Return the TCP port this Tox instance is bound to. This is only relevant if
-- the instance is acting as a TCP relay.
foreign import ccall tox_self_get_tcp_port :: Tox a -> CErr ErrGetPort -> IO Word16
callSelfGetTcpPort :: (Tox a -> CErr ErrGetPort -> IO Word16) ->
                        Tox a -> IO (Either ErrGetPort Word16)
callSelfGetTcpPort f tox = callErrFun $ f tox

toxSelfGetTcpPort :: Tox a -> IO (Either ErrGetPort Word16)
toxSelfGetTcpPort = callSelfGetTcpPort tox_self_get_udp_port
