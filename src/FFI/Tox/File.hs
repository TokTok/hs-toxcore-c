{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.File where

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

toxFileIdLength :: ToxPtr -> IO Word32
toxFileIdLength this = return FFI.Tox.Types.toxFileIdLength_Const
toxFileControlToString :: ToxPtr -> FileControl -> IO ByteString
toxFileControlToString this value = Raw.toxFileControlToString value >>= BS.packCString
toxFileControl :: ToxPtr -> FriendNumber -> FileNumber -> FileControl -> IO (Either ErrFileControl Bool)
toxFileControl this (FriendNumber friendNumber) (FileNumber id1) control = callErrFun (\errPtr -> Raw.toxFileControl this friendNumber id1 control (castPtr errPtr))
toxFileSeek :: ToxPtr -> FriendNumber -> FileNumber -> Word64 -> IO (Either ErrFileSeek Bool)
toxFileSeek this (FriendNumber friendNumber) (FileNumber id1) position = callErrFun (\errPtr -> Raw.toxFileSeek this friendNumber id1 (fromIntegral position) (castPtr errPtr))
toxFileGetFileId :: ToxPtr -> FriendNumber -> FileNumber -> IO (Either ErrFileGet ByteString)
toxFileGetFileId this (FriendNumber friendNumber) (FileNumber id1) = do { let { size  = FFI.Tox.Types.toxFileIdLength_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxFileGetFileId this friendNumber id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxFileSend :: ToxPtr -> FriendNumber -> Word32 -> Word64 -> ByteString -> ByteString -> IO (Either ErrFileSend FileNumber)
toxFileSend this (FriendNumber friendNumber) kind fileSize fileId filename = BS.useAsCString fileId (\ptr3 -> BS.useAsCStringLen filename (\(ptr4, len4) -> fmap (fmap FileNumber) (callErrFun (\errPtr -> Raw.toxFileSend this friendNumber (fromIntegral kind) (fromIntegral fileSize) (castPtr ptr3) (castPtr ptr4) (fromIntegral len4) (castPtr errPtr)))))
toxFileSendChunk :: ToxPtr -> FriendNumber -> FileNumber -> Word64 -> ByteString -> IO (Either ErrFileSendChunk Bool)
toxFileSendChunk this (FriendNumber friendNumber) (FileNumber id1) position data_ = BS.useAsCStringLen data_ (\(ptr2, len2) -> callErrFun (\errPtr -> Raw.toxFileSendChunk this friendNumber id1 (fromIntegral position) (castPtr ptr2) (fromIntegral len2) (castPtr errPtr)))
toxCallbackFileRecv :: ToxPtr -> FunPtr FileRecvCb -> IO ()
toxCallbackFileRecv this callback = Raw.toxCallbackFileRecv this (castFunPtrToPtr callback)
toxCallbackFileRecvChunk :: ToxPtr -> FunPtr FileRecvChunkCb -> IO ()
toxCallbackFileRecvChunk this callback = Raw.toxCallbackFileRecvChunk this (castFunPtrToPtr callback)
toxCallbackFileChunkRequest :: ToxPtr -> FunPtr FileChunkRequestCb -> IO ()
toxCallbackFileChunkRequest this callback = Raw.toxCallbackFileChunkRequest this (castFunPtrToPtr callback)
toxCallbackFileRecvControl :: ToxPtr -> FunPtr FileRecvControlCb -> IO ()
toxCallbackFileRecvControl this callback = Raw.toxCallbackFileRecvControl this (castFunPtrToPtr callback)
