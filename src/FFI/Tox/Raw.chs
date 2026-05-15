{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
#include <tox/tox.h>
#include <tox/toxav.h>

module FFI.Tox.Raw where

import           Data.Word             (Word32)
import           Foreign.C.String      (CString)
import           Foreign.C.Types       (CInt, CShort, CSize, CUChar, CUInt,
                                        CULLong, CUShort)
import           Foreign.Marshal.Utils (fromBool, toBool)
import           Foreign.Ptr           (Ptr, castFunPtrToPtr, castPtr,
                                        castPtrToFunPtr)
import           Foreign.Storable      (Storable (..))
import           GHC.Generics          (Generic)

type VoidPtr = Ptr ()

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

ptrToPtr :: Ptr a -> Ptr b
ptrToPtr = castPtr

{# context lib="toxcore" #}

{# enum Tox_Conference_Type as ConferenceType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ConferenceType where
  sizeOf _ = {# sizeof Tox_Conference_Type #}
  alignment _ = {# alignof Tox_Conference_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Connection as Connection { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable Connection where
  sizeOf _ = {# sizeof Tox_Connection #}
  alignment _ = {# alignof Tox_Connection #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Bootstrap as ErrBootstrap { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrBootstrap where
  sizeOf _ = {# sizeof Tox_Err_Bootstrap #}
  alignment _ = {# alignof Tox_Err_Bootstrap #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_By_Id as ErrConferenceById { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceById where
  sizeOf _ = {# sizeof Tox_Err_Conference_By_Id #}
  alignment _ = {# alignof Tox_Err_Conference_By_Id #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_By_Uid as ErrConferenceByUid { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceByUid where
  sizeOf _ = {# sizeof Tox_Err_Conference_By_Uid #}
  alignment _ = {# alignof Tox_Err_Conference_By_Uid #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Delete as ErrConferenceDelete { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceDelete where
  sizeOf _ = {# sizeof Tox_Err_Conference_Delete #}
  alignment _ = {# alignof Tox_Err_Conference_Delete #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Get_Type as ErrConferenceGetType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceGetType where
  sizeOf _ = {# sizeof Tox_Err_Conference_Get_Type #}
  alignment _ = {# alignof Tox_Err_Conference_Get_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Invite as ErrConferenceInvite { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceInvite where
  sizeOf _ = {# sizeof Tox_Err_Conference_Invite #}
  alignment _ = {# alignof Tox_Err_Conference_Invite #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Join as ErrConferenceJoin { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceJoin where
  sizeOf _ = {# sizeof Tox_Err_Conference_Join #}
  alignment _ = {# alignof Tox_Err_Conference_Join #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_New as ErrConferenceNew { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceNew where
  sizeOf _ = {# sizeof Tox_Err_Conference_New #}
  alignment _ = {# alignof Tox_Err_Conference_New #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Peer_Query as ErrConferencePeerQuery { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferencePeerQuery where
  sizeOf _ = {# sizeof Tox_Err_Conference_Peer_Query #}
  alignment _ = {# alignof Tox_Err_Conference_Peer_Query #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Send_Message as ErrConferenceSendMessage { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceSendMessage where
  sizeOf _ = {# sizeof Tox_Err_Conference_Send_Message #}
  alignment _ = {# alignof Tox_Err_Conference_Send_Message #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Set_Max_Offline as ErrConferenceSetMaxOffline { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceSetMaxOffline where
  sizeOf _ = {# sizeof Tox_Err_Conference_Set_Max_Offline #}
  alignment _ = {# alignof Tox_Err_Conference_Set_Max_Offline #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Conference_Title as ErrConferenceTitle { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrConferenceTitle where
  sizeOf _ = {# sizeof Tox_Err_Conference_Title #}
  alignment _ = {# alignof Tox_Err_Conference_Title #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_File_Control as ErrFileControl { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFileControl where
  sizeOf _ = {# sizeof Tox_Err_File_Control #}
  alignment _ = {# alignof Tox_Err_File_Control #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_File_Get as ErrFileGet { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFileGet where
  sizeOf _ = {# sizeof Tox_Err_File_Get #}
  alignment _ = {# alignof Tox_Err_File_Get #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_File_Seek as ErrFileSeek { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFileSeek where
  sizeOf _ = {# sizeof Tox_Err_File_Seek #}
  alignment _ = {# alignof Tox_Err_File_Seek #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_File_Send as ErrFileSend { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFileSend where
  sizeOf _ = {# sizeof Tox_Err_File_Send #}
  alignment _ = {# alignof Tox_Err_File_Send #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_File_Send_Chunk as ErrFileSendChunk { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFileSendChunk where
  sizeOf _ = {# sizeof Tox_Err_File_Send_Chunk #}
  alignment _ = {# alignof Tox_Err_File_Send_Chunk #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Add as ErrFriendAdd { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendAdd where
  sizeOf _ = {# sizeof Tox_Err_Friend_Add #}
  alignment _ = {# alignof Tox_Err_Friend_Add #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_By_Public_Key as ErrFriendByPublicKey { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendByPublicKey where
  sizeOf _ = {# sizeof Tox_Err_Friend_By_Public_Key #}
  alignment _ = {# alignof Tox_Err_Friend_By_Public_Key #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Custom_Packet as ErrFriendCustomPacket { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendCustomPacket where
  sizeOf _ = {# sizeof Tox_Err_Friend_Custom_Packet #}
  alignment _ = {# alignof Tox_Err_Friend_Custom_Packet #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Delete as ErrFriendDelete { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendDelete where
  sizeOf _ = {# sizeof Tox_Err_Friend_Delete #}
  alignment _ = {# alignof Tox_Err_Friend_Delete #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Get_Last_Online as ErrFriendGetLastOnline { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendGetLastOnline where
  sizeOf _ = {# sizeof Tox_Err_Friend_Get_Last_Online #}
  alignment _ = {# alignof Tox_Err_Friend_Get_Last_Online #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Get_Public_Key as ErrFriendGetPublicKey { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendGetPublicKey where
  sizeOf _ = {# sizeof Tox_Err_Friend_Get_Public_Key #}
  alignment _ = {# alignof Tox_Err_Friend_Get_Public_Key #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Query as ErrFriendQuery { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendQuery where
  sizeOf _ = {# sizeof Tox_Err_Friend_Query #}
  alignment _ = {# alignof Tox_Err_Friend_Query #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Friend_Send_Message as ErrFriendSendMessage { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrFriendSendMessage where
  sizeOf _ = {# sizeof Tox_Err_Friend_Send_Message #}
  alignment _ = {# alignof Tox_Err_Friend_Send_Message #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Get_Port as ErrGetPort { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGetPort where
  sizeOf _ = {# sizeof Tox_Err_Get_Port #}
  alignment _ = {# alignof Tox_Err_Get_Port #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Disconnect as ErrGroupDisconnect { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupDisconnect where
  sizeOf _ = {# sizeof Tox_Err_Group_Disconnect #}
  alignment _ = {# alignof Tox_Err_Group_Disconnect #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Invite_Accept as ErrGroupInviteAccept { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupInviteAccept where
  sizeOf _ = {# sizeof Tox_Err_Group_Invite_Accept #}
  alignment _ = {# alignof Tox_Err_Group_Invite_Accept #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Invite_Friend as ErrGroupInviteFriend { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupInviteFriend where
  sizeOf _ = {# sizeof Tox_Err_Group_Invite_Friend #}
  alignment _ = {# alignof Tox_Err_Group_Invite_Friend #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Is_Connected as ErrGroupIsConnected { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupIsConnected where
  sizeOf _ = {# sizeof Tox_Err_Group_Is_Connected #}
  alignment _ = {# alignof Tox_Err_Group_Is_Connected #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Join as ErrGroupJoin { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupJoin where
  sizeOf _ = {# sizeof Tox_Err_Group_Join #}
  alignment _ = {# alignof Tox_Err_Group_Join #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Kick_Peer as ErrGroupKickPeer { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupKickPeer where
  sizeOf _ = {# sizeof Tox_Err_Group_Kick_Peer #}
  alignment _ = {# alignof Tox_Err_Group_Kick_Peer #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Leave as ErrGroupLeave { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupLeave where
  sizeOf _ = {# sizeof Tox_Err_Group_Leave #}
  alignment _ = {# alignof Tox_Err_Group_Leave #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_New as ErrGroupNew { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupNew where
  sizeOf _ = {# sizeof Tox_Err_Group_New #}
  alignment _ = {# alignof Tox_Err_Group_New #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Peer_Query as ErrGroupPeerQuery { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupPeerQuery where
  sizeOf _ = {# sizeof Tox_Err_Group_Peer_Query #}
  alignment _ = {# alignof Tox_Err_Group_Peer_Query #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Reconnect as ErrGroupReconnect { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupReconnect where
  sizeOf _ = {# sizeof Tox_Err_Group_Reconnect #}
  alignment _ = {# alignof Tox_Err_Group_Reconnect #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Self_Name_Set as ErrGroupSelfNameSet { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSelfNameSet where
  sizeOf _ = {# sizeof Tox_Err_Group_Self_Name_Set #}
  alignment _ = {# alignof Tox_Err_Group_Self_Name_Set #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Self_Query as ErrGroupSelfQuery { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSelfQuery where
  sizeOf _ = {# sizeof Tox_Err_Group_Self_Query #}
  alignment _ = {# alignof Tox_Err_Group_Self_Query #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Self_Status_Set as ErrGroupSelfStatusSet { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSelfStatusSet where
  sizeOf _ = {# sizeof Tox_Err_Group_Self_Status_Set #}
  alignment _ = {# alignof Tox_Err_Group_Self_Status_Set #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Send_Custom_Packet as ErrGroupSendCustomPacket { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSendCustomPacket where
  sizeOf _ = {# sizeof Tox_Err_Group_Send_Custom_Packet #}
  alignment _ = {# alignof Tox_Err_Group_Send_Custom_Packet #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Send_Custom_Private_Packet as ErrGroupSendCustomPrivatePacket { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSendCustomPrivatePacket where
  sizeOf _ = {# sizeof Tox_Err_Group_Send_Custom_Private_Packet #}
  alignment _ = {# alignof Tox_Err_Group_Send_Custom_Private_Packet #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Send_Message as ErrGroupSendMessage { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSendMessage where
  sizeOf _ = {# sizeof Tox_Err_Group_Send_Message #}
  alignment _ = {# alignof Tox_Err_Group_Send_Message #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Send_Private_Message as ErrGroupSendPrivateMessage { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSendPrivateMessage where
  sizeOf _ = {# sizeof Tox_Err_Group_Send_Private_Message #}
  alignment _ = {# alignof Tox_Err_Group_Send_Private_Message #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Ignore as ErrGroupSetIgnore { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetIgnore where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Ignore #}
  alignment _ = {# alignof Tox_Err_Group_Set_Ignore #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Password as ErrGroupSetPassword { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetPassword where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Password #}
  alignment _ = {# alignof Tox_Err_Group_Set_Password #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Peer_Limit as ErrGroupSetPeerLimit { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetPeerLimit where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Peer_Limit #}
  alignment _ = {# alignof Tox_Err_Group_Set_Peer_Limit #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Privacy_State as ErrGroupSetPrivacyState { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetPrivacyState where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Privacy_State #}
  alignment _ = {# alignof Tox_Err_Group_Set_Privacy_State #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Role as ErrGroupSetRole { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetRole where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Role #}
  alignment _ = {# alignof Tox_Err_Group_Set_Role #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Topic_Lock as ErrGroupSetTopicLock { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetTopicLock where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Topic_Lock #}
  alignment _ = {# alignof Tox_Err_Group_Set_Topic_Lock #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Set_Voice_State as ErrGroupSetVoiceState { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupSetVoiceState where
  sizeOf _ = {# sizeof Tox_Err_Group_Set_Voice_State #}
  alignment _ = {# alignof Tox_Err_Group_Set_Voice_State #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_State_Query as ErrGroupStateQuery { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupStateQuery where
  sizeOf _ = {# sizeof Tox_Err_Group_State_Query #}
  alignment _ = {# alignof Tox_Err_Group_State_Query #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Group_Topic_Set as ErrGroupTopicSet { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrGroupTopicSet where
  sizeOf _ = {# sizeof Tox_Err_Group_Topic_Set #}
  alignment _ = {# alignof Tox_Err_Group_Topic_Set #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_New as ErrNew { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrNew where
  sizeOf _ = {# sizeof Tox_Err_New #}
  alignment _ = {# alignof Tox_Err_New #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Options_New as ErrOptionsNew { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrOptionsNew where
  sizeOf _ = {# sizeof Tox_Err_Options_New #}
  alignment _ = {# alignof Tox_Err_Options_New #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Set_Info as ErrSetInfo { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrSetInfo where
  sizeOf _ = {# sizeof Tox_Err_Set_Info #}
  alignment _ = {# alignof Tox_Err_Set_Info #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Err_Set_Typing as ErrSetTyping { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ErrSetTyping where
  sizeOf _ = {# sizeof Tox_Err_Set_Typing #}
  alignment _ = {# alignof Tox_Err_Set_Typing #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_File_Control as FileControl { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable FileControl where
  sizeOf _ = {# sizeof Tox_File_Control #}
  alignment _ = {# alignof Tox_File_Control #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_File_Kind as FileKind { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable FileKind where
  sizeOf _ = {# sizeof uint32_t #}
  alignment _ = {# alignof uint32_t #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Exit_Type as GroupExitType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupExitType where
  sizeOf _ = {# sizeof Tox_Group_Exit_Type #}
  alignment _ = {# alignof Tox_Group_Exit_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Join_Fail as GroupJoinFail { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupJoinFail where
  sizeOf _ = {# sizeof Tox_Group_Join_Fail #}
  alignment _ = {# alignof Tox_Group_Join_Fail #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Mod_Event as GroupModEvent { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupModEvent where
  sizeOf _ = {# sizeof Tox_Group_Mod_Event #}
  alignment _ = {# alignof Tox_Group_Mod_Event #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Privacy_State as GroupPrivacyState { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupPrivacyState where
  sizeOf _ = {# sizeof Tox_Group_Privacy_State #}
  alignment _ = {# alignof Tox_Group_Privacy_State #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Role as GroupRole { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupRole where
  sizeOf _ = {# sizeof Tox_Group_Role #}
  alignment _ = {# alignof Tox_Group_Role #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Topic_Lock as GroupTopicLock { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupTopicLock where
  sizeOf _ = {# sizeof Tox_Group_Topic_Lock #}
  alignment _ = {# alignof Tox_Group_Topic_Lock #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Group_Voice_State as GroupVoiceState { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable GroupVoiceState where
  sizeOf _ = {# sizeof Tox_Group_Voice_State #}
  alignment _ = {# alignof Tox_Group_Voice_State #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Log_Level as LogLevel { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable LogLevel where
  sizeOf _ = {# sizeof Tox_Log_Level #}
  alignment _ = {# alignof Tox_Log_Level #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Message_Type as MessageType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable MessageType where
  sizeOf _ = {# sizeof Tox_Message_Type #}
  alignment _ = {# alignof Tox_Message_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Proxy_Type as ProxyType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ProxyType where
  sizeOf _ = {# sizeof Tox_Proxy_Type #}
  alignment _ = {# alignof Tox_Proxy_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_Savedata_Type as SavedataType { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable SavedataType where
  sizeOf _ = {# sizeof Tox_Savedata_Type #}
  alignment _ = {# alignof Tox_Savedata_Type #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Tox_User_Status as UserStatus { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable UserStatus where
  sizeOf _ = {# sizeof Tox_User_Status #}
  alignment _ = {# alignof Tox_User_Status #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Call_Control as ToxavCallControl { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavCallControl where
  sizeOf _ = {# sizeof Toxav_Call_Control #}
  alignment _ = {# alignof Toxav_Call_Control #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_Answer as ToxavErrAnswer { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrAnswer where
  sizeOf _ = {# sizeof Toxav_Err_Answer #}
  alignment _ = {# alignof Toxav_Err_Answer #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_Bit_Rate_Set as ToxavErrBitRateSet { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrBitRateSet where
  sizeOf _ = {# sizeof Toxav_Err_Bit_Rate_Set #}
  alignment _ = {# alignof Toxav_Err_Bit_Rate_Set #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_Call as ToxavErrCall { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrCall where
  sizeOf _ = {# sizeof Toxav_Err_Call #}
  alignment _ = {# alignof Toxav_Err_Call #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_Call_Control as ToxavErrCallControl { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrCallControl where
  sizeOf _ = {# sizeof Toxav_Err_Call_Control #}
  alignment _ = {# alignof Toxav_Err_Call_Control #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_New as ToxavErrNew { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrNew where
  sizeOf _ = {# sizeof Toxav_Err_New #}
  alignment _ = {# alignof Toxav_Err_New #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Err_Send_Frame as ToxavErrSendFrame { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavErrSendFrame where
  sizeOf _ = {# sizeof Toxav_Err_Send_Frame #}
  alignment _ = {# alignof Toxav_Err_Send_Frame #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
{# enum Toxav_Friend_Call_State as ToxavFriendCallState { underscoreToCase } deriving (Eq, Ord, Show, Generic) #}
instance Storable ToxavFriendCallState where
  sizeOf _ = {# sizeof uint32_t #}
  alignment _ = {# alignof uint32_t #}
  peek p = fmap (toEnum . fromIntegral) (peek (castPtr p :: Ptr CInt))
  poke p v = poke (castPtr p :: Ptr CInt) (fromIntegral (fromEnum v))
data AvStruct
type AvPtr = Ptr AvStruct

{# fun toxav_new as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr ToxavErrNew' } -> `AvPtr' ptrToPtr #}
{# fun toxav_kill as ^ { ptrToPtr `AvPtr' } -> `()'  #}
{# fun toxav_iteration_interval as ^ { ptrToPtr `AvPtr' } -> `CUInt'  #}
{# fun toxav_iterate as ^ { ptrToPtr `AvPtr' } -> `()'  #}
{# fun toxav_audio_iteration_interval as ^ { ptrToPtr `AvPtr' } -> `CUInt'  #}
{# fun toxav_audio_iterate as ^ { ptrToPtr `AvPtr' } -> `()'  #}
{# fun toxav_video_iteration_interval as ^ { ptrToPtr `AvPtr' } -> `CUInt'  #}
{# fun toxav_video_iterate as ^ { ptrToPtr `AvPtr' } -> `()'  #}
{# fun toxav_call as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', fromIntegral `CUInt', fromIntegral `CUInt', ptrToPtr `Ptr ToxavErrCall' } -> `Bool' toBool #}
{# fun toxav_answer as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', fromIntegral `CUInt', fromIntegral `CUInt', ptrToPtr `Ptr ToxavErrAnswer' } -> `Bool' toBool #}
{# fun toxav_call_control as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', enumToCInt `ToxavCallControl', ptrToPtr `Ptr ToxavErrCallControl' } -> `Bool' toBool #}
{# fun toxav_audio_send_frame as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', ptrToPtr `Ptr CShort', fromIntegral `CSize', fromIntegral `CUChar', fromIntegral `CUInt', ptrToPtr `Ptr ToxavErrSendFrame' } -> `Bool' toBool #}
{# fun toxav_audio_set_bit_rate as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', fromIntegral `CUInt', ptrToPtr `Ptr ToxavErrBitRateSet' } -> `Bool' toBool #}
{# fun toxav_video_send_frame as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', fromIntegral `CUShort', fromIntegral `CUShort', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ToxavErrSendFrame' } -> `Bool' toBool #}
{# fun toxav_video_set_bit_rate as ^ { ptrToPtr `AvPtr', fromIntegral `CUInt', fromIntegral `CUInt', ptrToPtr `Ptr ToxavErrBitRateSet' } -> `Bool' toBool #}
{# fun toxav_add_av_groupchat as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `CInt'  #}
{# fun toxav_join_av_groupchat as ^ { ptrToPtr `ToxPtr', fromIntegral `CUInt', ptrToPtr `Ptr CUChar', fromIntegral `CUShort', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `CInt'  #}
{# fun toxav_group_send_audio as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CShort', fromIntegral `CUInt', fromIntegral `CUChar', fromIntegral `CUInt' } -> `CInt'  #}
{# fun toxav_groupchat_enable_av as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `CInt'  #}
{# fun toxav_groupchat_disable_av as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32' } -> `CInt'  #}
{# fun toxav_groupchat_av_enabled as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32' } -> `Bool' toBool #}

{# fun toxav_callback_call as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}
{# fun toxav_callback_call_state as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}
{# fun toxav_callback_audio_bit_rate as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}
{# fun toxav_callback_video_bit_rate as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}
{# fun toxav_callback_audio_receive_frame as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}
{# fun toxav_callback_video_receive_frame as ^ { ptrToPtr `AvPtr', castPtrToFunPtr `Ptr ()', ptrToPtr `Ptr ()' } -> `()' #}

{# fun tox_conference_uid_size as ^ {  } -> `CUInt'  #}
{# fun tox_conference_id_size as ^ {  } -> `CUInt'  #}
{# fun tox_conference_type_to_string as ^ { enumToCInt `ConferenceType' } -> `CString' ptrToPtr #}
{# fun tox_conference_new as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr ErrConferenceNew' } -> `Word32' fromIntegral #}
{# fun tox_conference_delete as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrConferenceDelete' } -> `Bool' toBool #}
{# fun tox_conference_set_max_offline as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `CUInt', ptrToPtr `Ptr ErrConferenceSetMaxOffline' } -> `Bool' toBool #}
{# fun tox_conference_join as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrConferenceJoin' } -> `Word32' fromIntegral #}
{# fun tox_conference_send_message as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `MessageType', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrConferenceSendMessage' } -> `Bool' toBool #}
{# fun tox_conference_get_title_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrConferenceTitle' } -> `CSize' fromIntegral #}
{# fun tox_conference_get_title as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferenceTitle' } -> `Bool' toBool #}
{# fun tox_conference_set_title as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrConferenceTitle' } -> `Bool' toBool #}
{# fun tox_conference_get_chatlist_size as ^ { ptrToPtr `ToxPtr' } -> `CSize' fromIntegral #}
{# fun tox_conference_get_chatlist as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr Word32' } -> `()'  #}
{# fun tox_conference_get_type as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrConferenceGetType' } -> `ConferenceType' cIntToEnum #}
{# fun tox_conference_get_id as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar' } -> `Bool' toBool #}
{# fun tox_conference_by_id as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferenceById' } -> `Word32' fromIntegral #}

{# fun tox_conference_offline_peer_count as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `CUInt'  #}
{# fun tox_conference_offline_peer_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `CSize' fromIntegral #}
{# fun tox_conference_offline_peer_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `Bool' toBool #}
{# fun tox_conference_offline_peer_get_public_key as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `Bool' toBool #}
{# fun tox_conference_offline_peer_get_last_active as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `CULLong'  #}

{# fun tox_conference_peer_count as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `CUInt'  #}
{# fun tox_conference_peer_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `CSize' fromIntegral #}
{# fun tox_conference_peer_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `Bool' toBool #}
{# fun tox_conference_peer_get_public_key as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `Bool' toBool #}
{# fun tox_conference_peer_number_is_ours as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrConferencePeerQuery' } -> `Bool' toBool #}

{# fun tox_file_id_length as ^ {  } -> `CUInt'  #}
{# fun tox_file_control_to_string as ^ { enumToCInt `FileControl' } -> `CString' ptrToPtr #}
{# fun tox_file_control as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', enumToCInt `FileControl', ptrToPtr `Ptr ErrFileControl' } -> `Bool' toBool #}
{# fun tox_file_seek as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', fromIntegral `CULLong', ptrToPtr `Ptr ErrFileSeek' } -> `Bool' toBool #}
{# fun tox_file_get_file_id as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFileGet' } -> `Bool' toBool #}
{# fun tox_file_send as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `CUInt', fromIntegral `CULLong', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFileSend' } -> `Word32' fromIntegral #}
{# fun tox_file_send_chunk as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', fromIntegral `CULLong', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFileSendChunk' } -> `Bool' toBool #}

{# fun tox_friend_add as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFriendAdd' } -> `Word32' fromIntegral #}
{# fun tox_friend_add_norequest as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFriendAdd' } -> `Word32' fromIntegral #}
{# fun tox_friend_delete as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendDelete' } -> `Bool' toBool #}
{# fun tox_friend_by_public_key as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFriendByPublicKey' } -> `Word32' fromIntegral #}
{# fun tox_friend_exists as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32' } -> `Bool' toBool #}
{# fun tox_friend_get_public_key as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFriendGetPublicKey' } -> `Bool' toBool #}
{# fun tox_friend_get_last_online as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendGetLastOnline' } -> `CULLong'  #}
{# fun tox_friend_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendQuery' } -> `CSize' fromIntegral #}
{# fun tox_friend_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFriendQuery' } -> `Bool' toBool #}
{# fun tox_friend_get_status_message_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendQuery' } -> `CSize' fromIntegral #}
{# fun tox_friend_get_status_message as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrFriendQuery' } -> `Bool' toBool #}
{# fun tox_friend_get_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendQuery' } -> `UserStatus' cIntToEnum #}
{# fun tox_friend_get_connection_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendQuery' } -> `Connection' cIntToEnum #}
{# fun tox_friend_get_typing as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrFriendQuery' } -> `Bool' toBool #}
{# fun tox_friend_send_message as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `MessageType', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFriendSendMessage' } -> `Word32' fromIntegral #}
{# fun tox_friend_send_lossy_packet as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFriendCustomPacket' } -> `Bool' toBool #}
{# fun tox_friend_send_lossless_packet as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrFriendCustomPacket' } -> `Bool' toBool #}


{# fun tox_group_max_topic_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_part_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_message_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_custom_lossy_packet_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_custom_lossless_packet_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_group_name_length as ^ {  } -> `CUInt'  #}
{# fun tox_group_max_password_size as ^ {  } -> `CUInt'  #}
{# fun tox_group_chat_id_size as ^ {  } -> `CUInt'  #}
{# fun tox_group_privacy_state_to_string as ^ { enumToCInt `GroupPrivacyState' } -> `CString' ptrToPtr #}
{# fun tox_group_topic_lock_to_string as ^ { enumToCInt `GroupTopicLock' } -> `CString' ptrToPtr #}
{# fun tox_group_voice_state_to_string as ^ { enumToCInt `GroupVoiceState' } -> `CString' ptrToPtr #}
{# fun tox_group_role_to_string as ^ { enumToCInt `GroupRole' } -> `CString' ptrToPtr #}
{# fun tox_group_new as ^ { ptrToPtr `ToxPtr', enumToCInt `GroupPrivacyState', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupNew' } -> `Word32' fromIntegral #}
{# fun tox_group_join as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupJoin' } -> `Word32' fromIntegral #}
{# fun tox_group_is_connected as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupIsConnected' } -> `Bool' toBool #}
{# fun tox_group_disconnect as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupDisconnect' } -> `Bool' toBool #}
{# fun tox_group_leave as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupLeave' } -> `Bool' toBool #}
{# fun tox_group_self_set_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSelfNameSet' } -> `Bool' toBool #}
{# fun tox_group_self_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `CSize' fromIntegral #}
{# fun tox_group_self_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `Bool' toBool #}
{# fun tox_group_self_set_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `UserStatus', ptrToPtr `Ptr ErrGroupSelfStatusSet' } -> `Bool' toBool #}
{# fun tox_group_self_get_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `UserStatus' cIntToEnum #}
{# fun tox_group_self_get_role as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `GroupRole' cIntToEnum #}
{# fun tox_group_self_get_peer_id as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `Word32' fromIntegral #}
{# fun tox_group_self_get_public_key as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupSelfQuery' } -> `Bool' toBool #}
{# fun tox_group_set_topic as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupTopicSet' } -> `Bool' toBool #}
{# fun tox_group_get_topic_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `CSize' fromIntegral #}
{# fun tox_group_get_topic as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupStateQuery' } -> `Bool' toBool #}
{# fun tox_group_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `CSize' fromIntegral #}
{# fun tox_group_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupStateQuery' } -> `Bool' toBool #}
{# fun tox_group_get_chat_id as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupStateQuery' } -> `Bool' toBool #}
{# fun tox_group_get_number_groups as ^ { ptrToPtr `ToxPtr' } -> `CUInt'  #}
{# fun tox_group_get_privacy_state as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `GroupPrivacyState' cIntToEnum #}
{# fun tox_group_get_voice_state as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `GroupVoiceState' cIntToEnum #}
{# fun tox_group_get_topic_lock as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `GroupTopicLock' cIntToEnum #}
{# fun tox_group_get_peer_limit as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `CUShort'  #}
{# fun tox_group_get_password_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupStateQuery' } -> `CSize' fromIntegral #}
{# fun tox_group_get_password as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupStateQuery' } -> `Bool' toBool #}
{# fun tox_group_send_message as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `MessageType', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSendMessage' } -> `Word32' fromIntegral #}
{# fun tox_group_send_private_message as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', enumToCInt `MessageType', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSendPrivateMessage' } -> `Word32' fromIntegral #}
{# fun tox_group_send_custom_packet as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromBool `Bool', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSendCustomPacket' } -> `Bool' toBool #}
{# fun tox_group_send_custom_private_packet as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', fromBool `Bool', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSendCustomPrivatePacket' } -> `Bool' toBool #}
{# fun tox_group_invite_friend as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupInviteFriend' } -> `Bool' toBool #}
{# fun tox_group_invite_accept as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupInviteAccept' } -> `Word32' fromIntegral #}
{# fun tox_group_exit_type_to_string as ^ { enumToCInt `GroupExitType' } -> `CString' ptrToPtr #}
{# fun tox_group_join_fail_to_string as ^ { enumToCInt `GroupJoinFail' } -> `CString' ptrToPtr #}
{# fun tox_group_set_password as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrGroupSetPassword' } -> `Bool' toBool #}
{# fun tox_group_set_topic_lock as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `GroupTopicLock', ptrToPtr `Ptr ErrGroupSetTopicLock' } -> `Bool' toBool #}
{# fun tox_group_set_voice_state as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `GroupVoiceState', ptrToPtr `Ptr ErrGroupSetVoiceState' } -> `Bool' toBool #}
{# fun tox_group_set_privacy_state as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', enumToCInt `GroupPrivacyState', ptrToPtr `Ptr ErrGroupSetPrivacyState' } -> `Bool' toBool #}
{# fun tox_group_set_peer_limit as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `CUShort', ptrToPtr `Ptr ErrGroupSetPeerLimit' } -> `Bool' toBool #}
{# fun tox_group_set_ignore as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', fromBool `Bool', ptrToPtr `Ptr ErrGroupSetIgnore' } -> `Bool' toBool #}
{# fun tox_group_set_role as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', enumToCInt `GroupRole', ptrToPtr `Ptr ErrGroupSetRole' } -> `Bool' toBool #}
{# fun tox_group_kick_peer as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupKickPeer' } -> `Bool' toBool #}
{# fun tox_group_mod_event_to_string as ^ { enumToCInt `GroupModEvent' } -> `CString' ptrToPtr #}


{# fun tox_group_peer_public_key_size as ^ {  } -> `CUInt'  #}
{# fun tox_group_peer_get_name_size as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `CSize' fromIntegral #}
{# fun tox_group_peer_get_name as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `Bool' toBool #}
{# fun tox_group_peer_get_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `UserStatus' cIntToEnum #}
{# fun tox_group_peer_get_role as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `GroupRole' cIntToEnum #}
{# fun tox_group_peer_get_connection_status as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `Connection' cIntToEnum #}
{# fun tox_group_peer_get_public_key as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrGroupPeerQuery' } -> `Bool' toBool #}

{# fun tox_callback_self_connection_status as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_name as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_status_message as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_status as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_connection_status as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_typing as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_read_receipt as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_request as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_message as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_file_recv as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_file_recv_chunk as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_file_chunk_request as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_file_recv_control as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_invite as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_connected as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_message as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_title as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_peer_name as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_conference_peer_list_changed as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_lossy_packet as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_friend_lossless_packet as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_invite as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_message as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_private_message as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_custom_packet as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_custom_private_packet as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_peer_join as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_peer_exit as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_self_join as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_join_fail as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_moderation as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_peer_name as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_peer_status as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_topic as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_privacy_state as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_voice_state as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_topic_lock as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_peer_limit as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}
{# fun tox_callback_group_password as ^ { ptrToPtr `ToxPtr', castPtrToFunPtr `Ptr ()' } -> `()' #}

data OptionsStruct
type OptionsPtr = Ptr OptionsStruct

{# fun tox_options_get_ipv6_enabled as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_ipv6_enabled as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_udp_enabled as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_udp_enabled as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_local_discovery_enabled as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_local_discovery_enabled as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_dht_announcements_enabled as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_dht_announcements_enabled as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_proxy_type as ^ { ptrToPtr `OptionsPtr' } -> `ProxyType' cIntToEnum #}
{# fun tox_options_set_proxy_type as ^ { ptrToPtr `OptionsPtr', enumToCInt `ProxyType' } -> `()'  #}
{# fun tox_options_get_proxy_host as ^ { ptrToPtr `OptionsPtr' } -> `CString' ptrToPtr #}
{# fun tox_options_set_proxy_host as ^ { ptrToPtr `OptionsPtr', ptrToPtr `CString' } -> `Bool' toBool #}
{# fun tox_options_get_proxy_port as ^ { ptrToPtr `OptionsPtr' } -> `CUShort'  #}
{# fun tox_options_set_proxy_port as ^ { ptrToPtr `OptionsPtr', fromIntegral `CUShort' } -> `()'  #}
{# fun tox_options_get_start_port as ^ { ptrToPtr `OptionsPtr' } -> `CUShort'  #}
{# fun tox_options_set_start_port as ^ { ptrToPtr `OptionsPtr', fromIntegral `CUShort' } -> `()'  #}
{# fun tox_options_get_end_port as ^ { ptrToPtr `OptionsPtr' } -> `CUShort'  #}
{# fun tox_options_set_end_port as ^ { ptrToPtr `OptionsPtr', fromIntegral `CUShort' } -> `()'  #}
{# fun tox_options_get_tcp_port as ^ { ptrToPtr `OptionsPtr' } -> `CUShort'  #}
{# fun tox_options_set_tcp_port as ^ { ptrToPtr `OptionsPtr', fromIntegral `CUShort' } -> `()'  #}
{# fun tox_options_get_hole_punching_enabled as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_hole_punching_enabled as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_savedata_type as ^ { ptrToPtr `OptionsPtr' } -> `SavedataType' cIntToEnum #}
{# fun tox_options_set_savedata_type as ^ { ptrToPtr `OptionsPtr', enumToCInt `SavedataType' } -> `()'  #}
{# fun tox_options_get_savedata_data as ^ { ptrToPtr `OptionsPtr' } -> `Ptr CUChar' ptrToPtr #}
{# fun tox_options_set_savedata_data as ^ { ptrToPtr `OptionsPtr', ptrToPtr `Ptr CUChar', fromIntegral `CSize' } -> `Bool' toBool #}
{# fun tox_options_get_savedata_length as ^ { ptrToPtr `OptionsPtr' } -> `CSize' fromIntegral #}
{# fun tox_options_set_savedata_length as ^ { ptrToPtr `OptionsPtr', fromIntegral `CSize' } -> `()'  #}
{# fun tox_options_get_log_callback as ^ { ptrToPtr `OptionsPtr' } -> `Ptr ()' castFunPtrToPtr #}
{# fun tox_options_set_log_callback as ^ { ptrToPtr `OptionsPtr', castPtrToFunPtr `Ptr ()' } -> `()'  #}
{# fun tox_options_get_log_user_data as ^ { ptrToPtr `OptionsPtr' } -> `VoidPtr' ptrToPtr #}
{# fun tox_options_set_log_user_data as ^ { ptrToPtr `OptionsPtr', ptrToPtr `VoidPtr' } -> `()'  #}
{# fun tox_options_get_experimental_owned_data as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_experimental_owned_data as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_experimental_thread_safety as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_experimental_thread_safety as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_experimental_groups_persistence as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_experimental_groups_persistence as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_get_experimental_disable_dns as ^ { ptrToPtr `OptionsPtr' } -> `Bool' toBool #}
{# fun tox_options_set_experimental_disable_dns as ^ { ptrToPtr `OptionsPtr', fromBool `Bool' } -> `()'  #}
{# fun tox_options_default as ^ { ptrToPtr `OptionsPtr' } -> `()'  #}
{# fun tox_options_new as ^ { ptrToPtr `Ptr ErrOptionsNew' } -> `OptionsPtr' ptrToPtr #}
{# fun tox_options_free as ^ { ptrToPtr `OptionsPtr' } -> `()'  #}
data ToxStruct
type ToxPtr = Ptr ToxStruct

{# fun toxav_get_tox as ^ { ptrToPtr `AvPtr' } -> `ToxPtr' ptrToPtr #}
{# fun tox_version_major as ^ {  } -> `CUInt'  #}
{# fun tox_version_minor as ^ {  } -> `CUInt'  #}
{# fun tox_version_patch as ^ {  } -> `CUInt'  #}
{# fun tox_version_is_compatible as ^ { fromIntegral `CUInt', fromIntegral `CUInt', fromIntegral `CUInt' } -> `Bool' toBool #}
{# fun tox_public_key_size as ^ {  } -> `CUInt'  #}
{# fun tox_secret_key_size as ^ {  } -> `CUInt'  #}
{# fun tox_nospam_size as ^ {  } -> `CUInt'  #}
{# fun tox_address_size as ^ {  } -> `CUInt'  #}
{# fun tox_max_name_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_status_message_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_friend_request_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_message_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_custom_packet_size as ^ {  } -> `CUInt'  #}
{# fun tox_hash_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_filename_length as ^ {  } -> `CUInt'  #}
{# fun tox_max_hostname_length as ^ {  } -> `CUInt'  #}
{# fun tox_user_status_to_string as ^ { enumToCInt `UserStatus' } -> `CString' ptrToPtr #}
{# fun tox_message_type_to_string as ^ { enumToCInt `MessageType' } -> `CString' ptrToPtr #}
{# fun tox_err_new_to_string as ^ { enumToCInt `ErrNew' } -> `CString' ptrToPtr #}
{# fun tox_new as ^ { ptrToPtr `OptionsPtr', ptrToPtr `Ptr ErrNew' } -> `ToxPtr' ptrToPtr #}
{# fun tox_kill as ^ { ptrToPtr `ToxPtr' } -> `()'  #}
{# fun tox_get_savedata_size as ^ { ptrToPtr `ToxPtr' } -> `CSize' fromIntegral #}
{# fun tox_get_savedata as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_err_bootstrap_to_string as ^ { enumToCInt `ErrBootstrap' } -> `CString' ptrToPtr #}
{# fun tox_bootstrap as ^ { ptrToPtr `ToxPtr', ptrToPtr `CString', fromIntegral `CUShort', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrBootstrap' } -> `Bool' toBool #}
{# fun tox_add_tcp_relay as ^ { ptrToPtr `ToxPtr', ptrToPtr `CString', fromIntegral `CUShort', ptrToPtr `Ptr CUChar', ptrToPtr `Ptr ErrBootstrap' } -> `Bool' toBool #}
{# fun tox_connection_to_string as ^ { enumToCInt `Connection' } -> `CString' ptrToPtr #}
{# fun tox_self_get_connection_status as ^ { ptrToPtr `ToxPtr' } -> `Connection' cIntToEnum #}
{# fun tox_iteration_interval as ^ { ptrToPtr `ToxPtr' } -> `CUInt'  #}
{# fun tox_iterate as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr ()' } -> `()'  #}
{# fun tox_self_get_address as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_self_set_nospam as ^ { ptrToPtr `ToxPtr', fromIntegral `CUInt' } -> `()'  #}
{# fun tox_self_get_nospam as ^ { ptrToPtr `ToxPtr' } -> `CUInt'  #}
{# fun tox_self_get_public_key as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_self_get_secret_key as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_err_set_info_to_string as ^ { enumToCInt `ErrSetInfo' } -> `CString' ptrToPtr #}
{# fun tox_self_set_name as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrSetInfo' } -> `Bool' toBool #}
{# fun tox_self_get_name_size as ^ { ptrToPtr `ToxPtr' } -> `CSize' fromIntegral #}
{# fun tox_self_get_name as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_self_set_status_message as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar', fromIntegral `CSize', ptrToPtr `Ptr ErrSetInfo' } -> `Bool' toBool #}
{# fun tox_self_get_status_message_size as ^ { ptrToPtr `ToxPtr' } -> `CSize' fromIntegral #}
{# fun tox_self_get_status_message as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_self_set_status as ^ { ptrToPtr `ToxPtr', enumToCInt `UserStatus' } -> `()'  #}
{# fun tox_self_get_status as ^ { ptrToPtr `ToxPtr' } -> `UserStatus' cIntToEnum #}
{# fun tox_err_friend_add_to_string as ^ { enumToCInt `ErrFriendAdd' } -> `CString' ptrToPtr #}
{# fun tox_err_friend_delete_to_string as ^ { enumToCInt `ErrFriendDelete' } -> `CString' ptrToPtr #}
{# fun tox_err_friend_by_public_key_to_string as ^ { enumToCInt `ErrFriendByPublicKey' } -> `CString' ptrToPtr #}
{# fun tox_self_get_friend_list_size as ^ { ptrToPtr `ToxPtr' } -> `CSize' fromIntegral #}
{# fun tox_self_get_friend_list as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr Word32' } -> `()'  #}
{# fun tox_err_friend_get_public_key_to_string as ^ { enumToCInt `ErrFriendGetPublicKey' } -> `CString' ptrToPtr #}
{# fun tox_err_friend_get_last_online_to_string as ^ { enumToCInt `ErrFriendGetLastOnline' } -> `CString' ptrToPtr #}
{# fun tox_err_friend_query_to_string as ^ { enumToCInt `ErrFriendQuery' } -> `CString' ptrToPtr #}
{# fun tox_err_set_typing_to_string as ^ { enumToCInt `ErrSetTyping' } -> `CString' ptrToPtr #}
{# fun tox_self_set_typing as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromBool `Bool', ptrToPtr `Ptr ErrSetTyping' } -> `Bool' toBool #}
{# fun tox_err_friend_send_message_to_string as ^ { enumToCInt `ErrFriendSendMessage' } -> `CString' ptrToPtr #}
{# fun tox_hash as ^ { ptrToPtr `Ptr CUChar', ptrToPtr `Ptr CUChar', fromIntegral `CSize' } -> `Bool' toBool #}
{# fun tox_err_file_control_to_string as ^ { enumToCInt `ErrFileControl' } -> `CString' ptrToPtr #}
{# fun tox_err_file_seek_to_string as ^ { enumToCInt `ErrFileSeek' } -> `CString' ptrToPtr #}
{# fun tox_err_file_get_to_string as ^ { enumToCInt `ErrFileGet' } -> `CString' ptrToPtr #}
{# fun tox_err_file_send_to_string as ^ { enumToCInt `ErrFileSend' } -> `CString' ptrToPtr #}
{# fun tox_err_file_send_chunk_to_string as ^ { enumToCInt `ErrFileSendChunk' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_new_to_string as ^ { enumToCInt `ErrConferenceNew' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_delete_to_string as ^ { enumToCInt `ErrConferenceDelete' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_peer_query_to_string as ^ { enumToCInt `ErrConferencePeerQuery' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_set_max_offline_to_string as ^ { enumToCInt `ErrConferenceSetMaxOffline' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_invite_to_string as ^ { enumToCInt `ErrConferenceInvite' } -> `CString' ptrToPtr #}
{# fun tox_conference_invite as ^ { ptrToPtr `ToxPtr', fromIntegral `Word32', fromIntegral `Word32', ptrToPtr `Ptr ErrConferenceInvite' } -> `Bool' toBool #}
{# fun tox_err_conference_join_to_string as ^ { enumToCInt `ErrConferenceJoin' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_send_message_to_string as ^ { enumToCInt `ErrConferenceSendMessage' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_title_to_string as ^ { enumToCInt `ErrConferenceTitle' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_get_type_to_string as ^ { enumToCInt `ErrConferenceGetType' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_by_id_to_string as ^ { enumToCInt `ErrConferenceById' } -> `CString' ptrToPtr #}
{# fun tox_err_conference_by_uid_to_string as ^ { enumToCInt `ErrConferenceByUid' } -> `CString' ptrToPtr #}
{# fun tox_err_friend_custom_packet_to_string as ^ { enumToCInt `ErrFriendCustomPacket' } -> `CString' ptrToPtr #}
{# fun tox_err_get_port_to_string as ^ { enumToCInt `ErrGetPort' } -> `CString' ptrToPtr #}
{# fun tox_self_get_dht_id as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr CUChar' } -> `()'  #}
{# fun tox_self_get_udp_port as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr ErrGetPort' } -> `CUShort'  #}
{# fun tox_self_get_tcp_port as ^ { ptrToPtr `ToxPtr', ptrToPtr `Ptr ErrGetPort' } -> `CUShort'  #}
{# fun tox_err_group_new_to_string as ^ { enumToCInt `ErrGroupNew' } -> `CString' ptrToPtr #}
{# fun tox_err_group_join_to_string as ^ { enumToCInt `ErrGroupJoin' } -> `CString' ptrToPtr #}
{# fun tox_err_group_is_connected_to_string as ^ { enumToCInt `ErrGroupIsConnected' } -> `CString' ptrToPtr #}
{# fun tox_err_group_disconnect_to_string as ^ { enumToCInt `ErrGroupDisconnect' } -> `CString' ptrToPtr #}
{# fun tox_err_group_reconnect_to_string as ^ { enumToCInt `ErrGroupReconnect' } -> `CString' ptrToPtr #}
{# fun tox_err_group_leave_to_string as ^ { enumToCInt `ErrGroupLeave' } -> `CString' ptrToPtr #}
{# fun tox_err_group_self_query_to_string as ^ { enumToCInt `ErrGroupSelfQuery' } -> `CString' ptrToPtr #}
{# fun tox_err_group_self_name_set_to_string as ^ { enumToCInt `ErrGroupSelfNameSet' } -> `CString' ptrToPtr #}
{# fun tox_err_group_self_status_set_to_string as ^ { enumToCInt `ErrGroupSelfStatusSet' } -> `CString' ptrToPtr #}
{# fun tox_err_group_peer_query_to_string as ^ { enumToCInt `ErrGroupPeerQuery' } -> `CString' ptrToPtr #}
{# fun tox_err_group_state_query_to_string as ^ { enumToCInt `ErrGroupStateQuery' } -> `CString' ptrToPtr #}
{# fun tox_err_group_topic_set_to_string as ^ { enumToCInt `ErrGroupTopicSet' } -> `CString' ptrToPtr #}
{# fun tox_err_group_send_message_to_string as ^ { enumToCInt `ErrGroupSendMessage' } -> `CString' ptrToPtr #}
{# fun tox_err_group_send_private_message_to_string as ^ { enumToCInt `ErrGroupSendPrivateMessage' } -> `CString' ptrToPtr #}
{# fun tox_err_group_send_custom_packet_to_string as ^ { enumToCInt `ErrGroupSendCustomPacket' } -> `CString' ptrToPtr #}
{# fun tox_err_group_send_custom_private_packet_to_string as ^ { enumToCInt `ErrGroupSendCustomPrivatePacket' } -> `CString' ptrToPtr #}
{# fun tox_err_group_invite_friend_to_string as ^ { enumToCInt `ErrGroupInviteFriend' } -> `CString' ptrToPtr #}
{# fun tox_err_group_invite_accept_to_string as ^ { enumToCInt `ErrGroupInviteAccept' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_password_to_string as ^ { enumToCInt `ErrGroupSetPassword' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_topic_lock_to_string as ^ { enumToCInt `ErrGroupSetTopicLock' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_voice_state_to_string as ^ { enumToCInt `ErrGroupSetVoiceState' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_privacy_state_to_string as ^ { enumToCInt `ErrGroupSetPrivacyState' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_peer_limit_to_string as ^ { enumToCInt `ErrGroupSetPeerLimit' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_ignore_to_string as ^ { enumToCInt `ErrGroupSetIgnore' } -> `CString' ptrToPtr #}
{# fun tox_err_group_set_role_to_string as ^ { enumToCInt `ErrGroupSetRole' } -> `CString' ptrToPtr #}
{# fun tox_err_group_kick_peer_to_string as ^ { enumToCInt `ErrGroupKickPeer' } -> `CString' ptrToPtr #}
{# fun tox_log_level_to_string as ^ { enumToCInt `LogLevel' } -> `CString' ptrToPtr #}
{# fun tox_proxy_type_to_string as ^ { enumToCInt `ProxyType' } -> `CString' ptrToPtr #}
{# fun tox_savedata_type_to_string as ^ { enumToCInt `SavedataType' } -> `CString' ptrToPtr #}
{# fun tox_err_options_new_to_string as ^ { enumToCInt `ErrOptionsNew' } -> `CString' ptrToPtr #}
deriving instance Bounded ConferenceType
deriving instance Read ConferenceType
deriving instance Bounded Connection
deriving instance Read Connection
deriving instance Bounded ErrBootstrap
deriving instance Read ErrBootstrap
deriving instance Bounded ErrConferenceById
deriving instance Read ErrConferenceById
deriving instance Bounded ErrConferenceByUid
deriving instance Read ErrConferenceByUid
deriving instance Bounded ErrConferenceDelete
deriving instance Read ErrConferenceDelete
deriving instance Bounded ErrConferenceGetType
deriving instance Read ErrConferenceGetType
deriving instance Bounded ErrConferenceInvite
deriving instance Read ErrConferenceInvite
deriving instance Bounded ErrConferenceJoin
deriving instance Read ErrConferenceJoin
deriving instance Bounded ErrConferenceNew
deriving instance Read ErrConferenceNew
deriving instance Bounded ErrConferencePeerQuery
deriving instance Read ErrConferencePeerQuery
deriving instance Bounded ErrConferenceSendMessage
deriving instance Read ErrConferenceSendMessage
deriving instance Bounded ErrConferenceSetMaxOffline
deriving instance Read ErrConferenceSetMaxOffline
deriving instance Bounded ErrConferenceTitle
deriving instance Read ErrConferenceTitle
deriving instance Bounded ErrFileControl
deriving instance Read ErrFileControl
deriving instance Bounded ErrFileGet
deriving instance Read ErrFileGet
deriving instance Bounded ErrFileSeek
deriving instance Read ErrFileSeek
deriving instance Bounded ErrFileSend
deriving instance Read ErrFileSend
deriving instance Bounded ErrFileSendChunk
deriving instance Read ErrFileSendChunk
deriving instance Bounded ErrFriendAdd
deriving instance Read ErrFriendAdd
deriving instance Bounded ErrFriendByPublicKey
deriving instance Read ErrFriendByPublicKey
deriving instance Bounded ErrFriendCustomPacket
deriving instance Read ErrFriendCustomPacket
deriving instance Bounded ErrFriendDelete
deriving instance Read ErrFriendDelete
deriving instance Bounded ErrFriendGetLastOnline
deriving instance Read ErrFriendGetLastOnline
deriving instance Bounded ErrFriendGetPublicKey
deriving instance Read ErrFriendGetPublicKey
deriving instance Bounded ErrFriendQuery
deriving instance Read ErrFriendQuery
deriving instance Bounded ErrFriendSendMessage
deriving instance Read ErrFriendSendMessage
deriving instance Bounded ErrGetPort
deriving instance Read ErrGetPort
deriving instance Bounded ErrGroupDisconnect
deriving instance Read ErrGroupDisconnect
deriving instance Bounded ErrGroupInviteAccept
deriving instance Read ErrGroupInviteAccept
deriving instance Bounded ErrGroupInviteFriend
deriving instance Read ErrGroupInviteFriend
deriving instance Bounded ErrGroupIsConnected
deriving instance Read ErrGroupIsConnected
deriving instance Bounded ErrGroupJoin
deriving instance Read ErrGroupJoin
deriving instance Bounded ErrGroupKickPeer
deriving instance Read ErrGroupKickPeer
deriving instance Bounded ErrGroupLeave
deriving instance Read ErrGroupLeave
deriving instance Bounded ErrGroupNew
deriving instance Read ErrGroupNew
deriving instance Bounded ErrGroupPeerQuery
deriving instance Read ErrGroupPeerQuery
deriving instance Bounded ErrGroupReconnect
deriving instance Read ErrGroupReconnect
deriving instance Bounded ErrGroupSelfNameSet
deriving instance Read ErrGroupSelfNameSet
deriving instance Bounded ErrGroupSelfQuery
deriving instance Read ErrGroupSelfQuery
deriving instance Bounded ErrGroupSelfStatusSet
deriving instance Read ErrGroupSelfStatusSet
deriving instance Bounded ErrGroupSendCustomPacket
deriving instance Read ErrGroupSendCustomPacket
deriving instance Bounded ErrGroupSendCustomPrivatePacket
deriving instance Read ErrGroupSendCustomPrivatePacket
deriving instance Bounded ErrGroupSendMessage
deriving instance Read ErrGroupSendMessage
deriving instance Bounded ErrGroupSendPrivateMessage
deriving instance Read ErrGroupSendPrivateMessage
deriving instance Bounded ErrGroupSetIgnore
deriving instance Read ErrGroupSetIgnore
deriving instance Bounded ErrGroupSetPassword
deriving instance Read ErrGroupSetPassword
deriving instance Bounded ErrGroupSetPeerLimit
deriving instance Read ErrGroupSetPeerLimit
deriving instance Bounded ErrGroupSetPrivacyState
deriving instance Read ErrGroupSetPrivacyState
deriving instance Bounded ErrGroupSetRole
deriving instance Read ErrGroupSetRole
deriving instance Bounded ErrGroupSetTopicLock
deriving instance Read ErrGroupSetTopicLock
deriving instance Bounded ErrGroupSetVoiceState
deriving instance Read ErrGroupSetVoiceState
deriving instance Bounded ErrGroupStateQuery
deriving instance Read ErrGroupStateQuery
deriving instance Bounded ErrGroupTopicSet
deriving instance Read ErrGroupTopicSet
deriving instance Bounded ErrNew
deriving instance Read ErrNew
deriving instance Bounded ErrOptionsNew
deriving instance Read ErrOptionsNew
deriving instance Bounded ErrSetInfo
deriving instance Read ErrSetInfo
deriving instance Bounded ErrSetTyping
deriving instance Read ErrSetTyping
deriving instance Bounded FileControl
deriving instance Read FileControl
deriving instance Bounded FileKind
deriving instance Read FileKind
deriving instance Bounded GroupExitType
deriving instance Read GroupExitType
deriving instance Bounded GroupJoinFail
deriving instance Read GroupJoinFail
deriving instance Bounded GroupModEvent
deriving instance Read GroupModEvent
deriving instance Bounded GroupPrivacyState
deriving instance Read GroupPrivacyState
deriving instance Bounded GroupRole
deriving instance Read GroupRole
deriving instance Bounded GroupTopicLock
deriving instance Read GroupTopicLock
deriving instance Bounded GroupVoiceState
deriving instance Read GroupVoiceState
deriving instance Bounded LogLevel
deriving instance Read LogLevel
deriving instance Bounded MessageType
deriving instance Read MessageType
deriving instance Bounded ProxyType
deriving instance Read ProxyType
deriving instance Bounded SavedataType
deriving instance Read SavedataType
deriving instance Bounded UserStatus
deriving instance Read UserStatus
deriving instance Bounded ToxavCallControl
deriving instance Read ToxavCallControl
deriving instance Bounded ToxavErrAnswer
deriving instance Read ToxavErrAnswer
deriving instance Bounded ToxavErrBitRateSet
deriving instance Read ToxavErrBitRateSet
deriving instance Bounded ToxavErrCall
deriving instance Read ToxavErrCall
deriving instance Bounded ToxavErrCallControl
deriving instance Read ToxavErrCallControl
deriving instance Bounded ToxavErrNew
deriving instance Read ToxavErrNew
deriving instance Bounded ToxavErrSendFrame
deriving instance Read ToxavErrSendFrame
deriving instance Bounded ToxavFriendCallState
deriving instance Read ToxavFriendCallState
