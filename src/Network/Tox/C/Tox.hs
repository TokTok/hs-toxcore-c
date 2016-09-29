{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
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

import           Control.Applicative       ((<$>))
import           Control.Exception         (bracket)
import qualified Crypto.Saltine.Class      as Sodium (decode, encode)
import           Crypto.Saltine.Core.Box   (PublicKey, SecretKey)
import qualified Data.ByteString           as BS
import           Data.Word                 (Word16, Word32, Word64)
import           Foreign.C.String          (CString, withCString)
import           Foreign.C.Types           (CInt (..), CSize (..))
import           Foreign.Marshal.Array     (allocaArray)
import           Foreign.Ptr               (FunPtr, Ptr, nullPtr)
import           Test.QuickCheck.Arbitrary (Arbitrary (..),
                                            arbitraryBoundedEnum)

import           Network.Tox.C.CEnum
import           Network.Tox.C.Constants
import           Network.Tox.C.Options
import           Network.Tox.C.Type


--------------------------------------------------------------------------------
--
-- :: Global enumerations
--
--------------------------------------------------------------------------------


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
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Arbitrary MessageType where
  arbitrary = arbitraryBoundedEnum


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

withTox :: OptionsPtr -> (Tox a -> IO r) -> IO (Either ErrNew r)
withTox options f =
  bracket (toxNew options) (either (const $ return ()) tox_kill) $ \case
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
  -> Tox a -> String -> Word16 -> PublicKey -> IO (Either ErrBootstrap ())
callBootstrapFun f tox address port pubKey =
  withCString address $ \address' ->
    BS.useAsCString (Sodium.encode pubKey) $ \pubKey' ->
      callErrFun $ f tox address' (fromIntegral port) pubKey'

toxBootstrap :: Tox a -> String -> Word16 -> PublicKey -> IO (Either ErrBootstrap ())
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

toxAddTcpRelay :: Tox a -> String -> Word16 -> PublicKey -> IO (Either ErrBootstrap ())
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

instance Arbitrary Connection where
  arbitrary = arbitraryBoundedEnum


-- | @param connection_status Whether we are connected to the DHT.
type SelfConnectionStatusCb a = Tox a -> CEnum Connection -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapSelfConnectionStatusCb :: SelfConnectionStatusCb a -> IO (FunPtr (SelfConnectionStatusCb a))


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
foreign import ccall tox_callback_self_connection_status :: Tox a -> FunPtr (SelfConnectionStatusCb a) -> IO ()

-- | Return the time in milliseconds before tox_iterate() should be called again
-- for optimal performance.
foreign import ccall tox_iteration_interval :: Tox a -> IO Word32

-- | The main loop that needs to be run in intervals of tox_iteration_interval()
-- milliseconds.
foreign import ccall tox_iterate :: Tox a -> Ptr a -> IO ()


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
  allocaArray addrLen $ \addrArr -> do
    tox_self_get_public_key tox addrArr
    BS.packCStringLen (addrArr, addrLen)

-- | Set the 4-byte nospam part of the address.
--
-- @param nospam Any 32 bit unsigned integer.
foreign import ccall tox_self_set_nospam :: Tox a -> Word32 -> IO ()

-- | Get the 4-byte nospam part of the address.
foreign import ccall tox_self_get_nospam :: Tox a -> IO Word32

-- | Copy the Tox Public Key (long term) from the Tox object.
--
-- @param public_key A memory region of at least 'tox_public_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_public_key :: Tox a -> CString -> IO ()

toxSelfGetPublicKey :: Tox a -> IO PublicKey
toxSelfGetPublicKey tox =
  let pkLen = fromIntegral tox_public_key_size in
  allocaArray pkLen $ \pkArr -> do
    tox_self_get_public_key tox pkArr
    Just pk <- Sodium.decode <$> BS.packCStringLen (pkArr, pkLen)
    return pk

-- | Copy the Tox Secret Key from the Tox object.
--
-- @param secret_key A memory region of at least 'tox_secret_key_size' bytes. If
--   this parameter is 'nullPtr', this function has no effect.
foreign import ccall tox_self_get_secret_key :: Tox a -> CString -> IO ()

toxSelfGetSecretKey :: Tox a -> IO SecretKey
toxSelfGetSecretKey tox =
  let skLen = fromIntegral tox_secret_key_size in
  allocaArray skLen $ \skArr -> do
    tox_self_get_secret_key tox skArr
    Just sk <- Sodium.decode <$> BS.packCStringLen (skArr, skLen)
    return sk


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

-- | Set the client's status message.
--
-- Status message length cannot exceed 'tox_max_status_message_length'. If
-- length is 0, the status parameter is ignored (it can be 'nullPtr'), and the
-- user status is set back to empty.
foreign import ccall tox_self_set_status_message :: Tox a -> CString -> CSize -> CErr ErrSetInfo -> IO ()

-- | Set the client's user status.
--
-- @param user_status One of the user statuses listed in the enumeration above.
foreign import ccall tox_self_set_status :: Tox a -> CEnum UserStatus -> IO ()


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

-- | Checks if a friend with the given friend number exists and returns true if
-- it does.
foreign import ccall tox_friend_exists :: Tox a -> Word32 -> IO Bool

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


--------------------------------------------------------------------------------
--
-- :: Friend-specific state queries (can also be received through callbacks)
--
--------------------------------------------------------------------------------


-- | @param friend_number The friend number of the friend whose name changed.
-- @param name A byte array containing the same data as
--   tox_friend_get_name would write to its `name` parameter.
-- @param length A value equal to the return value of
--   tox_friend_get_name_size.
type FriendNameCb a = Tox a -> Word32 -> CString -> CSize -> Ptr a
foreign import ccall "wrapper" wrapFriendNameCb :: FriendNameCb a -> IO (FunPtr (FriendNameCb a))

-- | Set the callback for the `friend_name` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend changes their name.
foreign import ccall tox_callback_friend_name :: Tox a -> FunPtr (FriendNameCb a) -> IO ()


-- | @param friend_number The friend number of the friend whose status message
--   changed.
-- @param message A byte array containing the same data as
--   tox_friend_get_status_message would write to its `status_message`
--   parameter.
-- @param length A value equal to the return value of
--   tox_friend_get_status_message_size.
type FriendStatusMessageCb a = Tox a -> Word32 -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendStatusMessageCb :: FriendStatusMessageCb a -> IO (FunPtr (FriendStatusMessageCb a))

-- | Set the callback for the `friend_status_message` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when a friend changes their status message.
foreign import ccall tox_callback_friend_status_message :: Tox a -> FunPtr (FriendStatusMessageCb a) -> IO ()


-- | @param friend_number The friend number of the friend whose user status
--   changed.
-- @param status The new user status.
type FriendStatusCb a = Tox a -> Word32 -> CEnum UserStatus -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendStatusCb :: FriendStatusCb a -> IO (FunPtr (FriendStatusCb a))


-- | Set the callback for the `friend_status` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend changes their user status.
foreign import ccall tox_callback_friend_status :: Tox a -> FunPtr (FriendStatusCb a) -> IO ()


-- | @param friend_number The friend number of the friend whose connection
--   status changed.
-- @param connection_status The result of calling
--   tox_friend_get_connection_status on the passed friend_number.
type FriendConnectionStatusCb a = Tox a -> Word32 -> CEnum Connection -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendConnectionStatusCb :: FriendConnectionStatusCb a -> IO (FunPtr (FriendConnectionStatusCb a))

-- | Set the callback for the `friend_connection_status` event. Pass 'nullPtr'
-- to unset.
--
-- This event is triggered when a friend goes offline after having been online,
-- or when a friend goes online.
--
-- This callback is not called when adding friends. It is assumed that when
-- adding friends, their connection status is initially offline.
foreign import ccall tox_callback_friend_connection_status :: Tox a -> FunPtr (FriendConnectionStatusCb a) -> IO ()


-- | @param friend_number The friend number of the friend who started or stopped
--   typing.
-- @param is_typing The result of calling tox_friend_get_typing on the passed
--   friend_number.
type FriendTypingCb a = Tox a -> Word32 -> Bool -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendTypingCb :: FriendTypingCb a -> IO (FunPtr (FriendTypingCb a))

-- | Set the callback for the `friend_typing` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend starts or stops typing.
foreign import ccall tox_callback_friend_typing :: Tox a -> FunPtr (FriendTypingCb a) -> IO ()


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

-- | @param friend_number The friend number of the friend who received the
--   message.
-- @param message_id The message ID as returned from tox_friend_send_message
--   corresponding to the message sent.
type FriendReadReceiptCb a = Tox a -> Word32 -> Word32 -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendReadReceiptCb :: FriendReadReceiptCb a -> IO (FunPtr (FriendReadReceiptCb a))


-- | Set the callback for the `friend_read_receipt` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when the friend receives the message sent with
-- tox_friend_send_message with the corresponding message ID.
foreign import ccall tox_callback_friend_read_receipt :: Tox a -> FunPtr (FriendReadReceiptCb a) -> IO ()


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
type FriendRequestCb a = Tox a -> CString -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendRequestCb :: FriendRequestCb a -> IO (FunPtr (FriendRequestCb a))


-- | Set the callback for the `friend_request` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a friend request is received.
foreign import ccall tox_callback_friend_request :: Tox a -> FunPtr (FriendRequestCb a) -> IO ()

-- | @param friend_number The friend number of the friend who sent the message.
-- @param time_delta Time between composition and sending.
-- @param message The message data they sent.
-- @param length The size of the message byte array.
--
-- @see friend_request for more information on time_delta.
type FriendMessageCb a = Tox a -> Word32 -> CEnum MessageType -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendMessageCb :: FriendMessageCb a -> IO (FunPtr (FriendMessageCb a))


-- | Set the callback for the `friend_message` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a message from a friend is received.
foreign import ccall tox_callback_friend_message :: Tox a -> FunPtr (FriendMessageCb a) -> IO ()


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

instance Arbitrary FileKind where
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
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Arbitrary FileControl where
  arbitrary = arbitraryBoundedEnum


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

-- | When receiving FileControlCancel, the client should release the
-- resources associated with the file number and consider the transfer failed.
--
-- @param friend_number The friend number of the friend who is sending the file.
-- @param file_number The friend-specific file number the data received is
--   associated with.
-- @param control The file control command received.
type FileRecvControlCb a = Tox a -> Word32 -> Word32 -> CEnum FileControl -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFileRecvControlCb :: FileRecvControlCb a -> IO (FunPtr (FileRecvControlCb a))


-- | Set the callback for the `file_recv_control` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when a file control command is received from a
-- friend.
foreign import ccall tox_callback_file_recv_control :: Tox a -> FunPtr (FileRecvControlCb a) -> IO ()

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
foreign import ccall tox_file_send :: Tox a -> Word32 -> Word32 -> Word64 -> CString -> CString -> CSize -> CErr ErrFileSend -> IO Word32

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
type FileChunkRequestCb a = Tox a -> Word32 -> Word32 -> Word64 -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFileChunkRequestCb :: FileChunkRequestCb a -> IO (FunPtr (FileChunkRequestCb a))


-- | Set the callback for the `file_chunk_request` event. Pass 'nullPtr' to
-- unset.
--
-- This event is triggered when Core is ready to send more file data.
foreign import ccall tox_callback_file_chunk_request :: Tox a -> FunPtr (FileChunkRequestCb a) -> IO ()


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
type FileRecvCb a = Tox a -> Word32 -> Word32 -> Word32 -> Word64 -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFileRecvCb :: FileRecvCb a -> IO (FunPtr (FileRecvCb a))


-- | Set the callback for the `file_recv` event. Pass 'nullPtr' to unset.
--
-- This event is triggered when a file transfer request is received.
foreign import ccall tox_callback_file_recv :: Tox a -> FunPtr (FileRecvCb a) -> IO ()

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
type FileRecvChunkCb a = Tox a -> Word32 -> Word32 -> Word64 -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFileRecvChunkCb :: FileRecvChunkCb a -> IO (FunPtr (FileRecvChunkCb a))


-- | Set the callback for the `file_recv_chunk` event. Pass 'nullPtr' to unset.
--
-- This event is first triggered when a file transfer request is received, and
-- subsequently when a chunk of file data for an accepted request was received.
foreign import ccall tox_callback_file_recv_chunk :: Tox a -> FunPtr (FileRecvChunkCb a) -> IO ()


--------------------------------------------------------------------------------
--
-- :: Group chat management
--
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--
-- :: Group chat message sending and receiving
--
--------------------------------------------------------------------------------


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

-- | @param friend_number The friend number of the friend who sent a lossy
-- packet.
-- @param data A byte array containing the received packet data.
-- @param length The length of the packet data byte array.
type FriendLossyPacketCb a = Tox a -> Word32 -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendLossyPacketCb :: FriendLossyPacketCb a -> IO (FunPtr (FriendLossyPacketCb a))


-- | Set the callback for the `friend_lossy_packet` event. Pass 'nullPtr' to
-- unset.
--
foreign import ccall tox_callback_friend_lossy_packet :: Tox a -> FunPtr (FriendLossyPacketCb a) -> IO ()

-- | @param friend_number The friend number of the friend who sent the packet.
-- @param data A byte array containing the received packet data.
-- @param length The length of the packet data byte array.
type FriendLosslessPacketCb a = Tox a -> Word32 -> CString -> CSize -> Ptr a -> IO ()
foreign import ccall "wrapper" wrapFriendLosslessPacketCb :: FriendLosslessPacketCb a -> IO (FunPtr (FriendLosslessPacketCb a))


-- | Set the callback for the `friend_lossless_packet` event. Pass 'nullPtr' to
-- unset.
--
foreign import ccall tox_callback_friend_lossless_packet :: Tox a -> FunPtr (FriendLosslessPacketCb a) -> IO ()


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

data ErrGetPort
  = ErrGetPortOk
    -- The function returned successfully.

  | ErrGetPortNotBound
    -- The instance was not bound to any port.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Return the UDP port this Tox instance is bound to.
foreign import ccall tox_self_get_udp_port :: Tox a -> CErr ErrGetPort -> IO Word16

-- | Return the TCP port this Tox instance is bound to. This is only relevant if
-- the instance is acting as a TCP relay.
foreign import ccall tox_self_get_tcp_port :: Tox a -> CErr ErrGetPort -> IO Word16
