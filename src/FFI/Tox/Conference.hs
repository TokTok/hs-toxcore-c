{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Tox.Conference where

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

toxConferenceUidSize :: ToxPtr -> IO Word32
toxConferenceUidSize this = return FFI.Tox.Types.toxConferenceUidSize_Const
toxConferenceIdSize :: ToxPtr -> IO Word32
toxConferenceIdSize this = return FFI.Tox.Types.toxConferenceIdSize_Const
toxConferenceTypeToString :: ToxPtr -> ConferenceType -> IO ByteString
toxConferenceTypeToString this value = Raw.toxConferenceTypeToString value >>= BS.packCString
toxConferenceNew :: ToxPtr -> IO (Either ErrConferenceNew ConferenceNumber)
toxConferenceNew this = fmap (fmap ConferenceNumber) (callErrFun (\errPtr -> Raw.toxConferenceNew this (castPtr errPtr)))
toxConferenceDelete :: ToxPtr -> ConferenceNumber -> IO (Either ErrConferenceDelete Bool)
toxConferenceDelete this (ConferenceNumber id1) = callErrFun (\errPtr -> Raw.toxConferenceDelete this id1 (castPtr errPtr))
toxConferenceSetMaxOffline :: ToxPtr -> ConferenceNumber -> Word32 -> IO (Either ErrConferenceSetMaxOffline Bool)
toxConferenceSetMaxOffline this (ConferenceNumber id1) maxOffline = callErrFun (\errPtr -> Raw.toxConferenceSetMaxOffline this id1 (fromIntegral maxOffline) (castPtr errPtr))
toxConferenceJoin :: ToxPtr -> FriendNumber -> ByteString -> IO (Either ErrConferenceJoin ConferenceNumber)
toxConferenceJoin this (FriendNumber friendNumber) cookie = BS.useAsCStringLen cookie (\(ptr1, len1) -> fmap (fmap ConferenceNumber) (callErrFun (\errPtr -> Raw.toxConferenceJoin this friendNumber (castPtr ptr1) (fromIntegral len1) (castPtr errPtr))))
toxConferenceSendMessage :: ToxPtr -> ConferenceNumber -> MessageType -> ByteString -> IO (Either ErrConferenceSendMessage Bool)
toxConferenceSendMessage this (ConferenceNumber id1) type_ message = BS.useAsCStringLen message (\(ptr1, len1) -> callErrFun (\errPtr -> Raw.toxConferenceSendMessage this id1 type_ (castPtr ptr1) (fromIntegral len1) (castPtr errPtr)))
toxConferenceGetTitleSize :: ToxPtr -> ConferenceNumber -> IO (Either ErrConferenceTitle CSize)
toxConferenceGetTitleSize this (ConferenceNumber id1) = fmap (fmap fromIntegral) (callErrFun (\errPtr -> Raw.toxConferenceGetTitleSize this id1 (castPtr errPtr)))
toxConferenceGetTitle :: ToxPtr -> ConferenceNumber -> IO (Either ErrConferenceTitle ByteString)
toxConferenceGetTitle this (ConferenceNumber id1) = do { size <- Raw.toxConferenceGetTitleSize this id1 nullPtr; allocaArray (fromIntegral size) (\outPtr -> do { res <- callErrFun (\errPtr -> Raw.toxConferenceGetTitle this id1 outPtr (castPtr errPtr));
    case res of
        (Left err) -> return (Left err)
        (Right _) -> do { val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return (Right val) }
 }) }
toxConferenceSetTitle :: ToxPtr -> ConferenceNumber -> ByteString -> IO (Either ErrConferenceTitle Bool)
toxConferenceSetTitle this (ConferenceNumber id1) title = BS.useAsCStringLen title (\(ptr0, len0) -> callErrFun (\errPtr -> Raw.toxConferenceSetTitle this id1 (castPtr ptr0) (fromIntegral len0) (castPtr errPtr)))
toxConferenceGetChatlistSize :: ToxPtr -> IO CSize
toxConferenceGetChatlistSize this = fmap fromIntegral (Raw.toxConferenceGetChatlistSize this)
toxConferenceGetChatlist :: ToxPtr -> IO [ConferenceNumber]
toxConferenceGetChatlist this = do { size <- Raw.toxConferenceGetChatlistSize this; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxConferenceGetChatlist this outPtr; val <- peekArray (fromIntegral size) (castPtr outPtr); return val }) }
toxConferenceGetType :: ToxPtr -> ConferenceNumber -> IO (Either ErrConferenceGetType ConferenceType)
toxConferenceGetType this (ConferenceNumber id1) = callErrFun (\errPtr -> Raw.toxConferenceGetType this id1 (castPtr errPtr))
toxConferenceGetId :: ToxPtr -> ConferenceNumber -> IO ByteString
toxConferenceGetId this (ConferenceNumber id1) = do { let { size  = FFI.Tox.Types.toxConferenceIdSize_Const }; allocaArray (fromIntegral size) (\outPtr -> do { _ <- Raw.toxConferenceGetId this id1 outPtr; val <- BS.packCStringLen (castPtr outPtr, fromIntegral size); return val }) }
toxConferenceById :: ToxPtr -> ByteString -> IO (Either ErrConferenceById ConferenceNumber)
toxConferenceById this conferenceId = BS.useAsCString conferenceId (\ptr0 -> fmap (fmap ConferenceNumber) (callErrFun (\errPtr -> Raw.toxConferenceById this (castPtr ptr0) (castPtr errPtr))))
toxCallbackConferenceInvite :: ToxPtr -> FunPtr ConferenceInviteCb -> IO ()
toxCallbackConferenceInvite this callback = Raw.toxCallbackConferenceInvite this (castFunPtrToPtr callback)
toxCallbackConferenceConnected :: ToxPtr -> FunPtr ConferenceConnectedCb -> IO ()
toxCallbackConferenceConnected this callback = Raw.toxCallbackConferenceConnected this (castFunPtrToPtr callback)
toxCallbackConferenceMessage :: ToxPtr -> FunPtr ConferenceMessageCb -> IO ()
toxCallbackConferenceMessage this callback = Raw.toxCallbackConferenceMessage this (castFunPtrToPtr callback)
toxCallbackConferenceTitle :: ToxPtr -> FunPtr ConferenceTitleCb -> IO ()
toxCallbackConferenceTitle this callback = Raw.toxCallbackConferenceTitle this (castFunPtrToPtr callback)
toxCallbackConferencePeerName :: ToxPtr -> FunPtr ConferencePeerNameCb -> IO ()
toxCallbackConferencePeerName this callback = Raw.toxCallbackConferencePeerName this (castFunPtrToPtr callback)
toxCallbackConferencePeerListChanged :: ToxPtr -> FunPtr ConferencePeerListChangedCb -> IO ()
toxCallbackConferencePeerListChanged this callback = Raw.toxCallbackConferencePeerListChanged this (castFunPtrToPtr callback)
