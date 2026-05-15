{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.Friend where

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

toxFriendAdd :: ToxPtr -> ByteString -> ByteString -> IO (Either ErrFriendAdd FriendNumber)
toxFriendAdd this address message = BS.useAsCString address (\ptr0 -> BS.useAsCStringLen message (\(ptr1, len1) -> fmap (fmap FriendNumber) (callErrFun (\errPtr -> Raw.toxFriendAdd this (castPtr ptr0) (castPtr ptr1) (fromIntegral len1) (castPtr errPtr)))))
toxFriendAddNorequest :: ToxPtr -> ByteString -> IO (Either ErrFriendAdd FriendNumber)
toxFriendAddNorequest this publicKey = BS.useAsCString publicKey (\ptr0 -> fmap (fmap FriendNumber) (callErrFun (\errPtr -> Raw.toxFriendAddNorequest this (castPtr ptr0) (castPtr errPtr))))
toxFriendDelete :: ToxPtr -> FriendNumber -> IO (Either ErrFriendDelete Bool)
toxFriendDelete this (FriendNumber id1) = callErrFun (\errPtr -> Raw.toxFriendDelete this id1 (castPtr errPtr))
toxFriendByPublicKey :: ToxPtr -> ByteString -> IO (Either ErrFriendByPublicKey FriendNumber)
toxFriendByPublicKey this publicKey = BS.useAsCString publicKey (\ptr0 -> fmap (fmap FriendNumber) (callErrFun (\errPtr -> Raw.toxFriendByPublicKey this (castPtr ptr0) (castPtr errPtr))))
toxFriendExists :: ToxPtr -> FriendNumber -> IO Bool
toxFriendExists this (FriendNumber id1) = Raw.toxFriendExists this id1
toxFriendGetPublicKey :: ToxPtr -> FriendNumber -> IO (Either ErrFriendGetPublicKey ByteString)
toxFriendGetPublicKey this (FriendNumber id1) = do { let { size  = FFI.Tox.Types.toxPublicKeySize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxFriendGetPublicKey this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxFriendGetLastOnline :: ToxPtr -> FriendNumber -> IO (Either ErrFriendGetLastOnline Word64)
toxFriendGetLastOnline this (FriendNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxFriendGetLastOnline this id1 (castPtr errPtr)))
toxFriendGetNameSize :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery CSize)
toxFriendGetNameSize this (FriendNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxFriendGetNameSize this id1 (castPtr errPtr)))
toxFriendGetName :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery ByteString)
toxFriendGetName this (FriendNumber id1) = do { size <- Raw.toxFriendGetNameSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxFriendGetName this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxFriendGetStatusMessageSize :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery CSize)
toxFriendGetStatusMessageSize this (FriendNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxFriendGetStatusMessageSize this id1 (castPtr errPtr)))
toxFriendGetStatusMessage :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery ByteString)
toxFriendGetStatusMessage this (FriendNumber id1) = do { size <- Raw.toxFriendGetStatusMessageSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxFriendGetStatusMessage this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxFriendGetStatus :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery UserStatus)
toxFriendGetStatus this (FriendNumber id1) = callErrFun (\errPtr -> Raw.toxFriendGetStatus this id1 (castPtr errPtr))
toxFriendGetConnectionStatus :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery Connection)
toxFriendGetConnectionStatus this (FriendNumber id1) = callErrFun (\errPtr -> Raw.toxFriendGetConnectionStatus this id1 (castPtr errPtr))
toxFriendGetTyping :: ToxPtr -> FriendNumber -> IO (Either ErrFriendQuery Bool)
toxFriendGetTyping this (FriendNumber id1) = callErrFun (\errPtr -> Raw.toxFriendGetTyping this id1 (castPtr errPtr))
toxFriendSendMessage :: ToxPtr -> FriendNumber -> MessageType -> ByteString -> IO (Either ErrFriendSendMessage FriendMessageId)
toxFriendSendMessage this (FriendNumber id1) type_ message = BS.useAsCStringLen message (\(ptr1, len1) -> fmap (fmap FriendMessageId) (callErrFun (\errPtr -> Raw.toxFriendSendMessage this id1 type_ (castPtr ptr1) (fromIntegral len1) (castPtr errPtr))))
toxFriendSendLossyPacket :: ToxPtr -> FriendNumber -> ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendSendLossyPacket this (FriendNumber id1) data_ = BS.useAsCStringLen data_ (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxFriendSendLossyPacket this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxFriendSendLosslessPacket :: ToxPtr -> FriendNumber -> ByteString -> IO (Either ErrFriendCustomPacket Bool)
toxFriendSendLosslessPacket this (FriendNumber id1) data_ = BS.useAsCStringLen data_ (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxFriendSendLosslessPacket this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxCallbackFriendName :: ToxPtr -> FunPtr FriendNameCb -> IO ()
toxCallbackFriendName this callback = Raw.toxCallbackFriendName this (castFunPtrToPtr callback)
toxCallbackFriendStatusMessage :: ToxPtr -> FunPtr FriendStatusMessageCb -> IO ()
toxCallbackFriendStatusMessage this callback = Raw.toxCallbackFriendStatusMessage this (castFunPtrToPtr callback)
toxCallbackFriendStatus :: ToxPtr -> FunPtr FriendStatusCb -> IO ()
toxCallbackFriendStatus this callback = Raw.toxCallbackFriendStatus this (castFunPtrToPtr callback)
toxCallbackFriendConnectionStatus :: ToxPtr -> FunPtr FriendConnectionStatusCb -> IO ()
toxCallbackFriendConnectionStatus this callback = Raw.toxCallbackFriendConnectionStatus this (castFunPtrToPtr callback)
toxCallbackFriendTyping :: ToxPtr -> FunPtr FriendTypingCb -> IO ()
toxCallbackFriendTyping this callback = Raw.toxCallbackFriendTyping this (castFunPtrToPtr callback)
toxCallbackFriendReadReceipt :: ToxPtr -> FunPtr FriendReadReceiptCb -> IO ()
toxCallbackFriendReadReceipt this callback = Raw.toxCallbackFriendReadReceipt this (castFunPtrToPtr callback)
toxCallbackFriendRequest :: ToxPtr -> FunPtr FriendRequestCb -> IO ()
toxCallbackFriendRequest this callback = Raw.toxCallbackFriendRequest this (castFunPtrToPtr callback)
toxCallbackFriendMessage :: ToxPtr -> FunPtr FriendMessageCb -> IO ()
toxCallbackFriendMessage this callback = Raw.toxCallbackFriendMessage this (castFunPtrToPtr callback)
toxCallbackFriendLossyPacket :: ToxPtr -> FunPtr FriendLossyPacketCb -> IO ()
toxCallbackFriendLossyPacket this callback = Raw.toxCallbackFriendLossyPacket this (castFunPtrToPtr callback)
toxCallbackFriendLosslessPacket :: ToxPtr -> FunPtr FriendLosslessPacketCb -> IO ()
toxCallbackFriendLosslessPacket this callback = Raw.toxCallbackFriendLosslessPacket this (castFunPtrToPtr callback)
