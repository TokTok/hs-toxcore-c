{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.GroupPeer where

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

toxGroupPeerPublicKeySize :: ToxPtr -> GroupNumber -> IO Word32
toxGroupPeerPublicKeySize this id1 = return FFI.Tox.Types.toxGroupPeerPublicKeySize_Const
toxGroupPeerGetNameSize :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery CSize)
toxGroupPeerGetNameSize this (GroupNumber id1) (GroupPeerNumber id2) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxGroupPeerGetNameSize this id1 id2 (castPtr errPtr)))
toxGroupPeerGetName :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery ByteString)
toxGroupPeerGetName this (GroupNumber id1) (GroupPeerNumber id2) = do { size <- Raw.toxGroupPeerGetNameSize this id1 id2 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupPeerGetName this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxGroupPeerGetStatus :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery UserStatus)
toxGroupPeerGetStatus this (GroupNumber id1) (GroupPeerNumber id2) = callErrFun (\errPtr -> Raw.toxGroupPeerGetStatus this id1 id2 (castPtr errPtr))
toxGroupPeerGetRole :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery GroupRole)
toxGroupPeerGetRole this (GroupNumber id1) (GroupPeerNumber id2) = callErrFun (\errPtr -> Raw.toxGroupPeerGetRole this id1 id2 (castPtr errPtr))
toxGroupPeerGetConnectionStatus :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery Connection)
toxGroupPeerGetConnectionStatus this (GroupNumber id1) (GroupPeerNumber id2) = callErrFun (\errPtr -> Raw.toxGroupPeerGetConnectionStatus this id1 id2 (castPtr errPtr))
toxGroupPeerGetPublicKey :: ToxPtr -> GroupNumber -> GroupPeerNumber -> IO (Either ErrGroupPeerQuery ByteString)
toxGroupPeerGetPublicKey this (GroupNumber id1) (GroupPeerNumber id2) = do { let { size  = FFI.Tox.Types.toxGroupPeerPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxGroupPeerGetPublicKey this id1 id2 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
