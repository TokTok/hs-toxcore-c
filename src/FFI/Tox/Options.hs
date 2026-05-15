{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.Options where

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

toxOptionsGetIpv6Enabled :: OptionsPtr -> IO Bool
toxOptionsGetIpv6Enabled this = Raw.toxOptionsGetIpv6Enabled this
toxOptionsSetIpv6Enabled :: OptionsPtr -> Bool -> IO ()
toxOptionsSetIpv6Enabled this ipv6Enabled = Raw.toxOptionsSetIpv6Enabled this ipv6Enabled
toxOptionsGetUdpEnabled :: OptionsPtr -> IO Bool
toxOptionsGetUdpEnabled this = Raw.toxOptionsGetUdpEnabled this
toxOptionsSetUdpEnabled :: OptionsPtr -> Bool -> IO ()
toxOptionsSetUdpEnabled this udpEnabled = Raw.toxOptionsSetUdpEnabled this udpEnabled
toxOptionsGetLocalDiscoveryEnabled :: OptionsPtr -> IO Bool
toxOptionsGetLocalDiscoveryEnabled this = Raw.toxOptionsGetLocalDiscoveryEnabled this
toxOptionsSetLocalDiscoveryEnabled :: OptionsPtr -> Bool -> IO ()
toxOptionsSetLocalDiscoveryEnabled this localDiscoveryEnabled = Raw.toxOptionsSetLocalDiscoveryEnabled this localDiscoveryEnabled
toxOptionsGetDhtAnnouncementsEnabled :: OptionsPtr -> IO Bool
toxOptionsGetDhtAnnouncementsEnabled this = Raw.toxOptionsGetDhtAnnouncementsEnabled this
toxOptionsSetDhtAnnouncementsEnabled :: OptionsPtr -> Bool -> IO ()
toxOptionsSetDhtAnnouncementsEnabled this dhtAnnouncementsEnabled = Raw.toxOptionsSetDhtAnnouncementsEnabled this dhtAnnouncementsEnabled
toxOptionsGetProxyType :: OptionsPtr -> IO ProxyType
toxOptionsGetProxyType this = Raw.toxOptionsGetProxyType this
toxOptionsSetProxyType :: OptionsPtr -> ProxyType -> IO ()
toxOptionsSetProxyType this proxyType = Raw.toxOptionsSetProxyType this proxyType
toxOptionsGetProxyHost :: OptionsPtr -> IO ByteString
toxOptionsGetProxyHost this = Raw.toxOptionsGetProxyHost this >>= BS.packCString
toxOptionsSetProxyHost :: OptionsPtr -> ByteString -> IO Bool
toxOptionsSetProxyHost this proxyHost = BS.useAsCString proxyHost (\ptr0 -> Raw.toxOptionsSetProxyHost this ptr0)
toxOptionsGetProxyPort :: OptionsPtr -> IO Word16
toxOptionsGetProxyPort this = fmap fromIntegral (Raw.toxOptionsGetProxyPort this)
toxOptionsSetProxyPort :: OptionsPtr -> Word16 -> IO ()
toxOptionsSetProxyPort this proxyPort = Raw.toxOptionsSetProxyPort this (fromIntegral proxyPort)
toxOptionsGetStartPort :: OptionsPtr -> IO Word16
toxOptionsGetStartPort this = fmap fromIntegral (Raw.toxOptionsGetStartPort this)
toxOptionsSetStartPort :: OptionsPtr -> Word16 -> IO ()
toxOptionsSetStartPort this startPort = Raw.toxOptionsSetStartPort this (fromIntegral startPort)
toxOptionsGetEndPort :: OptionsPtr -> IO Word16
toxOptionsGetEndPort this = fmap fromIntegral (Raw.toxOptionsGetEndPort this)
toxOptionsSetEndPort :: OptionsPtr -> Word16 -> IO ()
toxOptionsSetEndPort this endPort = Raw.toxOptionsSetEndPort this (fromIntegral endPort)
toxOptionsGetTcpPort :: OptionsPtr -> IO Word16
toxOptionsGetTcpPort this = fmap fromIntegral (Raw.toxOptionsGetTcpPort this)
toxOptionsSetTcpPort :: OptionsPtr -> Word16 -> IO ()
toxOptionsSetTcpPort this tcpPort = Raw.toxOptionsSetTcpPort this (fromIntegral tcpPort)
toxOptionsGetHolePunchingEnabled :: OptionsPtr -> IO Bool
toxOptionsGetHolePunchingEnabled this = Raw.toxOptionsGetHolePunchingEnabled this
toxOptionsSetHolePunchingEnabled :: OptionsPtr -> Bool -> IO ()
toxOptionsSetHolePunchingEnabled this holePunchingEnabled = Raw.toxOptionsSetHolePunchingEnabled this holePunchingEnabled
toxOptionsGetSavedataType :: OptionsPtr -> IO SavedataType
toxOptionsGetSavedataType this = Raw.toxOptionsGetSavedataType this
toxOptionsSetSavedataType :: OptionsPtr -> SavedataType -> IO ()
toxOptionsSetSavedataType this savedataType = Raw.toxOptionsSetSavedataType this savedataType
toxOptionsGetSavedataData :: OptionsPtr -> IO ByteString
toxOptionsGetSavedataData this = do
  ptr <- Raw.toxOptionsGetSavedataData this
  len <- Raw.toxOptionsGetSavedataLength this
  BS.packCStringLen (castPtr ptr, fromIntegral len)
toxOptionsSetSavedataData :: OptionsPtr -> ByteString -> IO Bool
toxOptionsSetSavedataData this savedataData = BS.useAsCStringLen savedataData (\(ptr0, len0) -> Raw.toxOptionsSetSavedataData this (castPtr ptr0) (fromIntegral len0))
toxOptionsGetSavedataLength :: OptionsPtr -> IO CSize
toxOptionsGetSavedataLength this = fmap fromIntegral (Raw.toxOptionsGetSavedataLength this)
toxOptionsSetSavedataLength :: OptionsPtr -> CSize -> IO ()
toxOptionsSetSavedataLength this savedataLength = Raw.toxOptionsSetSavedataLength this savedataLength
toxOptionsGetLogCallback :: OptionsPtr -> IO (FunPtr LogCb)
toxOptionsGetLogCallback this = fmap castPtrToFunPtr (Raw.toxOptionsGetLogCallback this)
toxOptionsSetLogCallback :: OptionsPtr -> FunPtr LogCb -> IO ()
toxOptionsSetLogCallback this logCallback = Raw.toxOptionsSetLogCallback this (castFunPtrToPtr logCallback)
toxOptionsGetLogUserData :: OptionsPtr -> IO (Ptr ())
toxOptionsGetLogUserData this = Raw.toxOptionsGetLogUserData this
toxOptionsSetLogUserData :: OptionsPtr -> Ptr () -> IO ()
toxOptionsSetLogUserData this logUserData = Raw.toxOptionsSetLogUserData this logUserData
toxOptionsGetExperimentalOwnedData :: OptionsPtr -> IO Bool
toxOptionsGetExperimentalOwnedData this = Raw.toxOptionsGetExperimentalOwnedData this
toxOptionsSetExperimentalOwnedData :: OptionsPtr -> Bool -> IO ()
toxOptionsSetExperimentalOwnedData this experimentalOwnedData = Raw.toxOptionsSetExperimentalOwnedData this experimentalOwnedData
toxOptionsGetExperimentalThreadSafety :: OptionsPtr -> IO Bool
toxOptionsGetExperimentalThreadSafety this = Raw.toxOptionsGetExperimentalThreadSafety this
toxOptionsSetExperimentalThreadSafety :: OptionsPtr -> Bool -> IO ()
toxOptionsSetExperimentalThreadSafety this experimentalThreadSafety = Raw.toxOptionsSetExperimentalThreadSafety this experimentalThreadSafety
toxOptionsGetExperimentalGroupsPersistence :: OptionsPtr -> IO Bool
toxOptionsGetExperimentalGroupsPersistence this = Raw.toxOptionsGetExperimentalGroupsPersistence this
toxOptionsSetExperimentalGroupsPersistence :: OptionsPtr -> Bool -> IO ()
toxOptionsSetExperimentalGroupsPersistence this experimentalGroupsPersistence = Raw.toxOptionsSetExperimentalGroupsPersistence this experimentalGroupsPersistence
toxOptionsGetExperimentalDisableDns :: OptionsPtr -> IO Bool
toxOptionsGetExperimentalDisableDns this = Raw.toxOptionsGetExperimentalDisableDns this
toxOptionsSetExperimentalDisableDns :: OptionsPtr -> Bool -> IO ()
toxOptionsSetExperimentalDisableDns this experimentalDisableDns = Raw.toxOptionsSetExperimentalDisableDns this experimentalDisableDns
toxOptionsDefault :: OptionsPtr -> IO ()
toxOptionsDefault this = Raw.toxOptionsDefault this
toxOptionsNew :: IO (Either ErrOptionsNew OptionsPtr)
toxOptionsNew  = callErrFun (\errPtr -> Raw.toxOptionsNew (castPtr errPtr))
toxOptionsFree :: OptionsPtr -> IO ()
toxOptionsFree this = Raw.toxOptionsFree this
