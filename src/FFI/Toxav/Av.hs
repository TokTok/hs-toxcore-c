{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module FFI.Toxav.Av where

import           Control.Monad         ((>=>))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Int              (Int16, Int32)
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
import           Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import           Foreign.Marshal.Utils (fromBool, toBool)
import           Foreign.Ptr           (FunPtr, Ptr, castFunPtrToPtr, castPtr,
                                        castPtrToFunPtr, nullPtr)
import           Foreign.Storable      (Storable)
import           GHC.Generics          (Generic)

toxavNew :: ToxPtr -> IO (Either ToxavErrNew AvPtr)
toxavNew this = callErrFun (\errPtr -> Raw.toxavNew this (castPtr errPtr))
toxavKill :: AvPtr -> IO ()
toxavKill this = Raw.toxavKill this
toxavIterationInterval :: AvPtr -> IO Word32
toxavIterationInterval this = fmap fromIntegral (Raw.toxavIterationInterval this)
toxavIterate :: AvPtr -> IO ()
toxavIterate this = Raw.toxavIterate this
toxavAudioIterationInterval :: AvPtr -> IO Word32
toxavAudioIterationInterval this = fmap fromIntegral (Raw.toxavAudioIterationInterval this)
toxavAudioIterate :: AvPtr -> IO ()
toxavAudioIterate this = Raw.toxavAudioIterate this
toxavVideoIterationInterval :: AvPtr -> IO Word32
toxavVideoIterationInterval this = fmap fromIntegral (Raw.toxavVideoIterationInterval this)
toxavVideoIterate :: AvPtr -> IO ()
toxavVideoIterate this = Raw.toxavVideoIterate this
toxavCall :: AvPtr -> Word32 -> Word32 -> Word32 -> IO (Either ToxavErrCall Bool)
toxavCall this friendNumber audioBitRate videoBitRate = callErrFun (\errPtr -> Raw.toxavCall this (fromIntegral friendNumber) (fromIntegral audioBitRate) (fromIntegral videoBitRate) (castPtr errPtr))
toxavAnswer :: AvPtr -> Word32 -> Word32 -> Word32 -> IO (Either ToxavErrAnswer Bool)
toxavAnswer this friendNumber audioBitRate videoBitRate = callErrFun (\errPtr -> Raw.toxavAnswer this (fromIntegral friendNumber) (fromIntegral audioBitRate) (fromIntegral videoBitRate) (castPtr errPtr))
toxavCallControl :: AvPtr -> Word32 -> ToxavCallControl -> IO (Either ToxavErrCallControl Bool)
toxavCallControl this friendNumber control = callErrFun (\errPtr -> Raw.toxavCallControl this (fromIntegral friendNumber) control (castPtr errPtr))
toxavAudioSendFrame :: AvPtr -> Word32 -> Ptr Int16 -> CSize -> Word8 -> Word32 -> IO (Either ToxavErrSendFrame Bool)
toxavAudioSendFrame this friendNumber pcm sampleCount channels samplingRate = callErrFun (\errPtr -> Raw.toxavAudioSendFrame this (fromIntegral friendNumber) (castPtr pcm) sampleCount (fromIntegral channels) (fromIntegral samplingRate) (castPtr errPtr))
toxavAudioSetBitRate :: AvPtr -> Word32 -> Word32 -> IO (Either ToxavErrBitRateSet Bool)
toxavAudioSetBitRate this friendNumber bitRate = callErrFun (\errPtr -> Raw.toxavAudioSetBitRate this (fromIntegral friendNumber) (fromIntegral bitRate) (castPtr errPtr))
toxavVideoSendFrame :: AvPtr -> Word32 -> Word16 -> Word16 -> ByteString -> ByteString -> ByteString -> IO (Either ToxavErrSendFrame Bool)
toxavVideoSendFrame this friendNumber width height y u v = BS.useAsCString y (\ptr3 -> BS.useAsCString u (\ptr4 -> BS.useAsCString v (\ptr5 -> callErrFun (\errPtr -> Raw.toxavVideoSendFrame this (fromIntegral friendNumber) (fromIntegral width) (fromIntegral height) (castPtr ptr3) (castPtr ptr4) (castPtr ptr5) (castPtr errPtr)))))
toxavVideoSetBitRate :: AvPtr -> Word32 -> Word32 -> IO (Either ToxavErrBitRateSet Bool)
toxavVideoSetBitRate this friendNumber bitRate = callErrFun (\errPtr -> Raw.toxavVideoSetBitRate this (fromIntegral friendNumber) (fromIntegral bitRate) (castPtr errPtr))
toxavAddAvGroupchat :: ToxPtr -> FunPtr ToxavAudioDataCb -> IO Int32
toxavAddAvGroupchat this audioCallback = fmap fromIntegral (Raw.toxavAddAvGroupchat this (castFunPtrToPtr audioCallback) nullPtr)
toxavJoinAvGroupchat :: ToxPtr -> Word32 -> ByteString -> Word16 -> FunPtr ToxavAudioDataCb -> IO Int32
toxavJoinAvGroupchat this friendnumber data_ length_ audioCallback = BS.useAsCStringLen data_ (\(ptr1, len1) -> fmap fromIntegral (Raw.toxavJoinAvGroupchat this (fromIntegral friendnumber) (castPtr ptr1) (fromIntegral length_) (castFunPtrToPtr audioCallback) nullPtr))
toxavGroupSendAudio :: ToxPtr -> ConferenceNumber -> [Int16] -> Word32 -> Word8 -> Word32 -> IO Int32
toxavGroupSendAudio this (ConferenceNumber groupnumber) pcm samples channels sampleRate = Foreign.Marshal.Array.withArray pcm (\pcmPtr -> fmap fromIntegral (Raw.toxavGroupSendAudio this groupnumber (castPtr pcmPtr) (fromIntegral samples) (fromIntegral channels) (fromIntegral sampleRate)))
toxavGroupchatEnableAv :: ToxPtr -> ConferenceNumber -> FunPtr ToxavAudioDataCb -> IO Int32
toxavGroupchatEnableAv this (ConferenceNumber groupnumber) audioCallback = fmap fromIntegral (Raw.toxavGroupchatEnableAv this groupnumber (castFunPtrToPtr audioCallback) nullPtr)
toxavGroupchatDisableAv :: ToxPtr -> ConferenceNumber -> IO Int32
toxavGroupchatDisableAv this (ConferenceNumber groupnumber) = fmap fromIntegral (Raw.toxavGroupchatDisableAv this groupnumber)
toxavGroupchatAvEnabled :: ToxPtr -> ConferenceNumber -> IO Bool
toxavGroupchatAvEnabled this (ConferenceNumber groupnumber) = Raw.toxavGroupchatAvEnabled this groupnumber
toxavCallbackCall :: AvPtr -> FunPtr ToxavCallCb -> Ptr () -> IO ()
toxavCallbackCall this callback userData = Raw.toxavCallbackCall this (castFunPtrToPtr callback) userData
toxavCallbackCallState :: AvPtr -> FunPtr ToxavCallStateCb -> Ptr () -> IO ()
toxavCallbackCallState this callback userData = Raw.toxavCallbackCallState this (castFunPtrToPtr callback) userData
toxavCallbackAudioBitRate :: AvPtr -> FunPtr ToxavAudioBitRateCb -> Ptr () -> IO ()
toxavCallbackAudioBitRate this callback userData = Raw.toxavCallbackAudioBitRate this (castFunPtrToPtr callback) userData
toxavCallbackVideoBitRate :: AvPtr -> FunPtr ToxavVideoBitRateCb -> Ptr () -> IO ()
toxavCallbackVideoBitRate this callback userData = Raw.toxavCallbackVideoBitRate this (castFunPtrToPtr callback) userData
toxavCallbackAudioReceiveFrame :: AvPtr -> FunPtr ToxavAudioReceiveFrameCb -> Ptr () -> IO ()
toxavCallbackAudioReceiveFrame this callback userData = Raw.toxavCallbackAudioReceiveFrame this (castFunPtrToPtr callback) userData
toxavCallbackVideoReceiveFrame :: AvPtr -> FunPtr ToxavVideoReceiveFrameCb -> Ptr () -> IO ()
toxavCallbackVideoReceiveFrame this callback userData = Raw.toxavCallbackVideoReceiveFrame this (castFunPtrToPtr callback) userData
