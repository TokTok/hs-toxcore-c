{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.ConferencePeer where

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

toxConferencePeerCount :: ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> IO (Either ErrConferencePeerQuery Word32)
toxConferencePeerCount this (ConferenceNumber id1) id2 = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferencePeerCount this id1 (castPtr errPtr)))
toxConferencePeerGetNameSize :: ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> IO (Either ErrConferencePeerQuery CSize)
toxConferencePeerGetNameSize this (ConferenceNumber id1) (ConferencePeerNumber id2) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferencePeerGetNameSize this id1 id2 (castPtr errPtr)))
toxConferencePeerGetName :: ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> IO (Either ErrConferencePeerQuery ByteString)
toxConferencePeerGetName this (ConferenceNumber id1) (ConferencePeerNumber id2) = do { size <- Raw.toxConferencePeerGetNameSize this id1 id2 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxConferencePeerGetName this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxConferencePeerGetPublicKey :: ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> IO (Either ErrConferencePeerQuery ByteString)
toxConferencePeerGetPublicKey this (ConferenceNumber id1) (ConferencePeerNumber id2) = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxConferencePeerGetPublicKey this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxConferencePeerNumberIsOurs :: ToxPtr -> ConferenceNumber -> ConferencePeerNumber -> IO (Either ErrConferencePeerQuery Bool)
toxConferencePeerNumberIsOurs this (ConferenceNumber id1) (ConferencePeerNumber id2) = callErrFun (\errPtr -> Raw.toxConferencePeerNumberIsOurs this id1 id2 (castPtr errPtr))
