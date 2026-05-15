{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.ConferenceOfflinePeer where

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

toxConferenceOfflinePeerCount :: ToxPtr -> ConferenceNumber -> ConferenceOfflinePeerNumber -> IO (Either ErrConferencePeerQuery Word32)
toxConferenceOfflinePeerCount this (ConferenceNumber id1) id2 = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferenceOfflinePeerCount this id1 (castPtr errPtr)))
toxConferenceOfflinePeerGetNameSize :: ToxPtr -> ConferenceNumber -> ConferenceOfflinePeerNumber -> IO (Either ErrConferencePeerQuery CSize)
toxConferenceOfflinePeerGetNameSize this (ConferenceNumber id1) (ConferenceOfflinePeerNumber id2) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferenceOfflinePeerGetNameSize this id1 id2 (castPtr errPtr)))
toxConferenceOfflinePeerGetName :: ToxPtr -> ConferenceNumber -> ConferenceOfflinePeerNumber -> IO (Either ErrConferencePeerQuery ByteString)
toxConferenceOfflinePeerGetName this (ConferenceNumber id1) (ConferenceOfflinePeerNumber id2) = do { size <- Raw.toxConferenceOfflinePeerGetNameSize this id1 id2 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxConferenceOfflinePeerGetName this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxConferenceOfflinePeerGetPublicKey :: ToxPtr -> ConferenceNumber -> ConferenceOfflinePeerNumber -> IO (Either ErrConferencePeerQuery ByteString)
toxConferenceOfflinePeerGetPublicKey this (ConferenceNumber id1) (ConferenceOfflinePeerNumber id2) = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxConferenceOfflinePeerGetPublicKey this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxConferenceOfflinePeerGetLastActive :: ToxPtr -> ConferenceNumber -> ConferenceOfflinePeerNumber -> IO (Either ErrConferencePeerQuery Word64)
toxConferenceOfflinePeerGetLastActive this (ConferenceNumber id1) (ConferenceOfflinePeerNumber id2) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferenceOfflinePeerGetLastActive this id1 id2 (castPtr errPtr)))
