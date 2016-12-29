module Network.Tox.C.Callbacks where

import           Control.Exception  (bracket)
import           Foreign.Ptr        (freeHaskellFunPtr, nullFunPtr)

import qualified Network.Tox.C.Tox  as Tox
import           Network.Tox.C.Type (Tox)


-- | Low level event handler. The functions in this class are directly
-- registered with the corresponding C callback. We use 'StablePtr' to pass
-- opaque Haskell values around in C.
class CHandler a where
  cSelfConnectionStatus     :: Tox.SelfConnectionStatusCb     a
  cSelfConnectionStatus _ _ ud = return ud
  cFriendName               :: Tox.FriendNameCb               a
  cFriendName _ _ _ ud = return ud
  cFriendStatusMessage      :: Tox.FriendStatusMessageCb      a
  cFriendStatusMessage _ _ _ ud = return ud
  cFriendStatus             :: Tox.FriendStatusCb             a
  cFriendStatus _ _ _ ud = return ud
  cFriendConnectionStatus   :: Tox.FriendConnectionStatusCb   a
  cFriendConnectionStatus _ _ _ ud = return ud
  cFriendTyping             :: Tox.FriendTypingCb             a
  cFriendTyping _ _ _ ud = return ud
  cFriendReadReceipt        :: Tox.FriendReadReceiptCb        a
  cFriendReadReceipt _ _ _ ud = return ud
  cFriendRequest            :: Tox.FriendRequestCb            a
  cFriendRequest _ _ _ ud = return ud
  cFriendMessage            :: Tox.FriendMessageCb            a
  cFriendMessage _ _ _ _ ud = return ud
  cFileRecvControl          :: Tox.FileRecvControlCb          a
  cFileRecvControl _ _ _ _ ud = return ud
  cFileChunkRequest         :: Tox.FileChunkRequestCb         a
  cFileChunkRequest _ _ _ _ _ ud = return ud
  cFileRecv                 :: Tox.FileRecvCb                 a
  cFileRecv _ _ _ _ _ _ ud = return ud
  cFileRecvChunk            :: Tox.FileRecvChunkCb            a
  cFileRecvChunk _ _ _ _ _ ud = return ud
  cConferenceInvite         :: Tox.ConferenceInviteCb         a
  cConferenceInvite _ _ _ _ ud = return ud
  cConferenceMessage        :: Tox.ConferenceMessageCb        a
  cConferenceMessage _ _ _ _ _ ud = return ud
  cConferenceTitle          :: Tox.ConferenceTitleCb          a
  cConferenceTitle _ _ _ _ ud = return ud
  cConferenceNamelistChange :: Tox.ConferenceNamelistChangeCb a
  cConferenceNamelistChange _ _ _ _ ud = return ud
  cFriendLossyPacket        :: Tox.FriendLossyPacketCb        a
  cFriendLossyPacket _ _ _ ud = return ud
  cFriendLosslessPacket     :: Tox.FriendLosslessPacketCb     a
  cFriendLosslessPacket _ _ _ ud = return ud


-- | Installs an event handler into the passed 'Tox' instance. After performing
-- the IO action, all event handlers are reset to null. This function does not
-- save the original event handlers.
withCHandler :: CHandler a => Tox a -> IO r -> IO r
withCHandler tox =
  install Tox.tox_callback_self_connection_status     (Tox.selfConnectionStatusCb     cSelfConnectionStatus     ) .
  install Tox.tox_callback_friend_name                (Tox.friendNameCb               cFriendName               ) .
  install Tox.tox_callback_friend_status_message      (Tox.friendStatusMessageCb      cFriendStatusMessage      ) .
  install Tox.tox_callback_friend_status              (Tox.friendStatusCb             cFriendStatus             ) .
  install Tox.tox_callback_friend_connection_status   (Tox.friendConnectionStatusCb   cFriendConnectionStatus   ) .
  install Tox.tox_callback_friend_typing              (Tox.friendTypingCb             cFriendTyping             ) .
  install Tox.tox_callback_friend_read_receipt        (Tox.friendReadReceiptCb        cFriendReadReceipt        ) .
  install Tox.tox_callback_friend_request             (Tox.friendRequestCb            cFriendRequest            ) .
  install Tox.tox_callback_friend_message             (Tox.friendMessageCb            cFriendMessage            ) .
  install Tox.tox_callback_file_recv_control          (Tox.fileRecvControlCb          cFileRecvControl          ) .
  install Tox.tox_callback_file_chunk_request         (Tox.fileChunkRequestCb         cFileChunkRequest         ) .
  install Tox.tox_callback_file_recv                  (Tox.fileRecvCb                 cFileRecv                 ) .
  install Tox.tox_callback_file_recv_chunk            (Tox.fileRecvChunkCb            cFileRecvChunk            ) .
  install Tox.tox_callback_conference_invite          (Tox.conferenceInviteCb         cConferenceInvite         ) .
  install Tox.tox_callback_conference_message         (Tox.conferenceMessageCb        cConferenceMessage        ) .
  install Tox.tox_callback_conference_title           (Tox.conferenceTitleCb          cConferenceTitle          ) .
  install Tox.tox_callback_conference_namelist_change (Tox.conferenceNamelistChangeCb cConferenceNamelistChange ) .
  install Tox.tox_callback_friend_lossy_packet        (Tox.friendLossyPacketCb        cFriendLossyPacket        ) .
  install Tox.tox_callback_friend_lossless_packet     (Tox.friendLosslessPacketCb     cFriendLosslessPacket     )
  where
    install cInstall wrapper action =
      bracket wrapper (uninstall cInstall) $ \cb -> do
        () <- cInstall tox cb
        action

    uninstall cInstall cb = do
      freeHaskellFunPtr cb
      cInstall tox nullFunPtr
