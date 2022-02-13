{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Safe          #-}
{-# LANGUAGE StrictData    #-}
module Network.Tox.C.Options where

import           Control.Exception   (bracket)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Default.Class  (Default (..))
import           Data.Word           (Word16, Word32)
import           Foreign.C.String    (CString, peekCString, withCString)
import           Foreign.C.Types     (CInt (..), CSize (..))
import           Foreign.Ptr         (FunPtr, Ptr, nullPtr)
import           GHC.Generics        (Generic)

import           Network.Tox.C.CEnum

--------------------------------------------------------------------------------
--
-- :: Startup options
--
--------------------------------------------------------------------------------


-- | Type of proxy used to connect to TCP relays.
data ProxyType
  = ProxyTypeNone
    -- Don't use a proxy.
  | ProxyTypeHttp
    -- HTTP proxy using CONNECT.
  | ProxyTypeSocks5
    -- SOCKS proxy for simple socket pipes.
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)


-- Type of savedata to create the Tox instance from.
data SavedataType
  = SavedataTypeNone
    -- No savedata.
  | SavedataTypeToxSave
    -- Savedata is one that was obtained from tox_get_savedata
  | SavedataTypeSecretKey
    -- Savedata is a secret key of length 'tox_secret_key_size'
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)


-- This struct contains all the startup options for Tox. You can either allocate
-- this object yourself, and pass it to tox_options_default, or call
-- tox_options_new to get a new default options object.
data Options = Options
  { ipv6Enabled  :: Bool
    -- The type of socket to create.
    --
    -- If this is set to false, an IPv4 socket is created, which subsequently
    -- only allows IPv4 communication.
    -- If it is set to true, an IPv6 socket is created, allowing both IPv4 and
    -- IPv6 communication.

  , udpEnabled   :: Bool
    -- Enable the use of UDP communication when available.
    --
    -- Setting this to false will force Tox to use TCP only. Communications will
    -- need to be relayed through a TCP relay node, potentially slowing them
    -- down. Disabling UDP support is necessary when using anonymous proxies or
    -- Tor.

  , proxyType    :: ProxyType
    -- Pass communications through a proxy.

  , proxyHost    :: String
    -- The IP address or DNS name of the proxy to be used.
    --
    -- If used, this must be non-'nullPtr' and be a valid DNS name. The name
    -- must not exceed 255 ('tox_max_filename_length') characters, and be in a
    -- NUL-terminated C string format (255 chars + 1 NUL byte).
    --
    -- This member is ignored (it can be 'nullPtr') if proxy_type is
    -- ProxyTypeNone.

  , proxyPort    :: Word16
    -- The port to use to connect to the proxy server.
    --
    -- Ports must be in the range (1, 65535). The value is ignored if proxy_type
    -- is ProxyTypeNone.

  , startPort    :: Word16
    -- The start port of the inclusive port range to attempt to use.
    --
    -- If both 'startPort' and 'endPort' are 0, the default port range will be
    -- used: [33445, 33545].
    --
    -- If either 'startPort' or 'endPort' is 0 while the other is non-zero, the
    -- non-zero port will be the only port in the range.
    --
    -- Having 'startPort' > 'endport' will yield the same behavior as if
    -- 'startPort' and 'endPort' were swapped.

  , endPort      :: Word16
    -- The end port of the inclusive port range to attempt to use.

  , tcpPort      :: Word16
    -- The port to use for the TCP server (relay). If 0, the TCP server is
    -- disabled.
    --
    -- Enabling it is not required for Tox to function properly.
    --
    -- When enabled, your Tox instance can act as a TCP relay for other Tox
    -- instance. This leads to increased traffic, thus when writing a client it
    -- is recommended to enable TCP server only if the user has an option to
    -- disable it.

  , savedataType :: SavedataType
    -- The type of savedata to load from.

  , savedataData :: ByteString
    -- The savedata bytes.
  }
  deriving (Eq, Read, Show, Generic)


instance Default Options where
  def = Options
    { ipv6Enabled  = True
    , udpEnabled   = True
    , proxyType    = ProxyTypeNone
    , proxyHost    = ""
    , proxyPort    = 0
    , startPort    = 0
    , endPort      = 0
    , tcpPort      = 0
    , savedataType = SavedataTypeNone
    , savedataData = BS.empty
    }


data OptionsStruct
type OptionsPtr = Ptr OptionsStruct


foreign import ccall tox_options_get_ipv6_enabled     :: OptionsPtr -> IO Bool
foreign import ccall tox_options_get_udp_enabled      :: OptionsPtr -> IO Bool
foreign import ccall tox_options_get_proxy_type       :: OptionsPtr -> IO (CEnum ProxyType)
foreign import ccall tox_options_get_proxy_host       :: OptionsPtr -> IO CString
foreign import ccall tox_options_get_proxy_port       :: OptionsPtr -> IO Word16
foreign import ccall tox_options_get_start_port       :: OptionsPtr -> IO Word16
foreign import ccall tox_options_get_end_port         :: OptionsPtr -> IO Word16
foreign import ccall tox_options_get_tcp_port         :: OptionsPtr -> IO Word16
foreign import ccall tox_options_get_savedata_type    :: OptionsPtr -> IO (CEnum SavedataType)
foreign import ccall tox_options_get_savedata_data    :: OptionsPtr -> IO CString
foreign import ccall tox_options_get_savedata_length  :: OptionsPtr -> IO CSize

foreign import ccall tox_options_set_ipv6_enabled     :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_set_udp_enabled      :: OptionsPtr -> Bool -> IO ()
foreign import ccall tox_options_set_proxy_type       :: OptionsPtr -> CEnum ProxyType -> IO ()
foreign import ccall tox_options_set_proxy_host       :: OptionsPtr -> CString -> IO ()
foreign import ccall tox_options_set_proxy_port       :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_set_start_port       :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_set_end_port         :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_set_tcp_port         :: OptionsPtr -> Word16 -> IO ()
foreign import ccall tox_options_set_savedata_type    :: OptionsPtr -> CEnum SavedataType -> IO ()
foreign import ccall tox_options_set_savedata_data    :: OptionsPtr -> CString -> CSize -> IO ()
foreign import ccall tox_options_set_savedata_length  :: OptionsPtr -> CSize -> IO ()


data LogLevel
  = LogLevelTrace
  | LogLevelDebug
  | LogLevelInfo
  | LogLevelWarning
  | LogLevelError
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

logLevelName :: LogLevel -> Char
logLevelName LogLevelTrace   = 'T'
logLevelName LogLevelDebug   = 'D'
logLevelName LogLevelInfo    = 'I'
logLevelName LogLevelWarning = 'W'
logLevelName LogLevelError   = 'E'

type LogCb = Ptr () -> CEnum LogLevel -> CString -> Word32 -> CString -> CString -> Ptr () -> IO ()
foreign import ccall tox_options_set_log_callback :: OptionsPtr -> FunPtr LogCb -> IO ()
foreign import ccall "wrapper" wrapLogCb :: LogCb -> IO (FunPtr LogCb)

logHandler :: LogCb
logHandler _ cLevel cFile line cFunc cMsg _ = do
    let level = fromCEnum cLevel
    file <- peekCString cFile
    func <- peekCString cFunc
    msg <- peekCString cMsg
    case level of
        LogLevelTrace -> return ()
        _ -> putStrLn $ logLevelName level : ' ' : file <> ":" <> show line <> "(" <> func <> "): " <> msg


data ErrOptionsNew
  = ErrOptionsNewMalloc
    -- The function was unable to allocate enough memory to store the internal
    -- structures for the Tox options object.
  deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- | Allocates a new Tox_Options object and initialises it with the default
-- options. This function can be used to preserve long term ABI compatibility by
-- giving the responsibility of allocation and deallocation to the Tox library.
--
-- Objects returned from this function must be freed using the tox_options_free
-- function.
--
-- @return A new 'OptionsPtr' with default options or 'nullPtr' on failure.
foreign import ccall tox_options_new :: CErr ErrOptionsNew -> IO OptionsPtr

toxOptionsNew :: IO (Either ErrOptionsNew OptionsPtr)
toxOptionsNew = callErrFun tox_options_new


-- | Releases all resources associated with an options objects.
--
-- Passing a pointer that was not returned by tox_options_new results in
-- undefined behaviour.
foreign import ccall tox_options_free :: OptionsPtr -> IO ()


withToxOptions :: (OptionsPtr -> IO r) -> IO (Either ErrOptionsNew r)
withToxOptions f =
  bracket toxOptionsNew (either (const $ return ()) tox_options_free) $ \case
    Left err -> return $ Left err
    Right ok -> Right <$> f ok


-- | Read 'Options' from an 'OptionsPtr'.
--
-- If the passed pointer is 'nullPtr', the behaviour is undefined.
peekToxOptions :: OptionsPtr -> IO Options
peekToxOptions ptr = do
  cIpv6Enabled    <- tox_options_get_ipv6_enabled    ptr
  cUdpEnabled     <- tox_options_get_udp_enabled     ptr
  cProxyType      <- tox_options_get_proxy_type      ptr
  cProxyHost      <- tox_options_get_proxy_host      ptr >>= peekNullableString
  cProxyPort      <- tox_options_get_proxy_port      ptr
  cStartPort      <- tox_options_get_start_port      ptr
  cEndPort        <- tox_options_get_end_port        ptr
  cTcpPort        <- tox_options_get_tcp_port        ptr
  cSavedataType   <- tox_options_get_savedata_type   ptr
  cSavedataData   <- tox_options_get_savedata_data   ptr
  cSavedataLength <- tox_options_get_savedata_length ptr
  cSavedata       <- BS.packCStringLen
                           ( cSavedataData
                           , fromIntegral cSavedataLength)
  return Options
    { ipv6Enabled    = cIpv6Enabled
    , udpEnabled     = cUdpEnabled
    , proxyType      = fromCEnum cProxyType
    , proxyHost      = cProxyHost
    , proxyPort      = cProxyPort
    , startPort      = cStartPort
    , endPort        = cEndPort
    , tcpPort        = cTcpPort
    , savedataType   = fromCEnum cSavedataType
    , savedataData   = cSavedata
    }

  where
    -- 'peekCString' doesn't handle NULL strings as empty, unlike
    -- 'packCStringLen', which ignores the pointer to zero-length 'CString's.
    peekNullableString p =
      if p == nullPtr
        then return ""
        else peekCString p


-- | Save the options from the passed 'OptionsPtr', perform an IO action, and
-- restore the original values.
--
-- If the passed pointer is 'nullPtr', the behaviour is undefined.
saveToxOptions :: OptionsPtr -> IO r -> IO r
saveToxOptions ptr =
  bracket saveOptions restoreOptions . const
  where
    saveOptions = do
      v0 <- tox_options_get_ipv6_enabled    ptr
      v1 <- tox_options_get_udp_enabled     ptr
      v2 <- tox_options_get_proxy_type      ptr
      v3 <- tox_options_get_proxy_host      ptr
      v4 <- tox_options_get_proxy_port      ptr
      v5 <- tox_options_get_start_port      ptr
      v6 <- tox_options_get_end_port        ptr
      v7 <- tox_options_get_tcp_port        ptr
      v8 <- tox_options_get_savedata_type   ptr
      sd <- tox_options_get_savedata_data   ptr
      sl <- tox_options_get_savedata_length ptr
      return (v0, v1, v2, v3, v4, v5, v6, v7, v8, sd, sl)

    restoreOptions (v0, v1, v2, v3, v4, v5, v6, v7, v8, sd, sl) = do
      tox_options_set_ipv6_enabled    ptr v0
      tox_options_set_udp_enabled     ptr v1
      tox_options_set_proxy_type      ptr v2
      tox_options_set_proxy_host      ptr v3
      tox_options_set_proxy_port      ptr v4
      tox_options_set_start_port      ptr v5
      tox_options_set_end_port        ptr v6
      tox_options_set_tcp_port        ptr v7
      tox_options_set_savedata_type   ptr v8
      tox_options_set_savedata_data   ptr sd sl
      tox_options_set_savedata_length ptr sl
      tox_options_set_log_callback    ptr =<< wrapLogCb logHandler


-- | Fill in the 'Options' values into the 'OptionsPtr' and perform the IO
-- action afterwards.
--
-- This function restores the original values from the 'OptionsPtr' after
-- performing the action.
--
-- If the passed pointer is 'nullPtr', the behaviour is undefined.
pokeToxOptions :: Options -> OptionsPtr -> IO r -> IO r
pokeToxOptions options ptr action =
  saveToxOptions ptr $
    withCString (proxyHost options) $ \host ->
      BS.useAsCStringLen (savedataData options) $ \(saveData, saveLenInt) -> do
        let saveLen = fromIntegral saveLenInt
        tox_options_set_ipv6_enabled    ptr $ ipv6Enabled options
        tox_options_set_udp_enabled     ptr $ udpEnabled options
        tox_options_set_proxy_type      ptr $ toCEnum $ proxyType options
        tox_options_set_proxy_host      ptr host
        tox_options_set_proxy_port      ptr $ proxyPort options
        tox_options_set_start_port      ptr $ startPort options
        tox_options_set_end_port        ptr $ endPort options
        tox_options_set_tcp_port        ptr $ tcpPort options
        tox_options_set_savedata_type   ptr $ toCEnum $ savedataType options
        tox_options_set_savedata_data   ptr saveData saveLen
        tox_options_set_savedata_length ptr saveLen
        tox_options_set_log_callback    ptr =<< wrapLogCb logHandler
        action


-- | Allocate a new C options pointer, fills in the values from 'Options',
-- calls the processor function, and deallocates the options pointer.
--
-- The 'OptionsPtr' passed to the processor function is never 'nullPtr'. If
-- allocation fails, the IO action evaluates to 'Left' with an appropriate
-- error code.
withOptions :: Options -> (OptionsPtr -> IO r) -> IO (Either ErrOptionsNew r)
withOptions options f =
  withToxOptions $ \ptr ->
    pokeToxOptions options ptr (f ptr)
