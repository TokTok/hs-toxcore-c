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
  cSelfConnectionStatus _ _ = return
  cFriendName               :: Tox.FriendNameCb               a
  cFriendName _ _ _ = return
  cFriendStatusMessage      :: Tox.FriendStatusMessageCb      a
  cFriendStatusMessage _ _ _ = return
  cFriendStatus             :: Tox.FriendStatusCb             a
  cFriendStatus _ _ _ = return
  cFriendConnectionStatus   :: Tox.FriendConnectionStatusCb   a
  cFriendConnectionStatus _ _ _ = return
  cFriendTyping             :: Tox.FriendTypingCb             a
  cFriendTyping _ _ _ = return
  cFriendReadReceipt        :: Tox.FriendReadReceiptCb        a
  cFriendReadReceipt _ _ _ = return
  cFriendRequest            :: Tox.FriendRequestCb            a
  cFriendRequest _ _ _ = return
  cFriendMessage            :: Tox.FriendMessageCb            a
  cFriendMessage _ _ _ _ = return
  cFileRecvControl          :: Tox.FileRecvControlCb          a
  cFileRecvControl _ _ _ _ = return
  cFileChunkRequest         :: Tox.FileChunkRequestCb         a
  cFileChunkRequest _ _ _ _ _ = return
  cFileRecv                 :: Tox.FileRecvCb                 a
  cFileRecv _ _ _ _ _ _ = return
  cFileRecvChunk            :: Tox.FileRecvChunkCb            a
  cFileRecvChunk _ _ _ _ _ = return
  cConferenceInvite         :: Tox.ConferenceInviteCb         a
  cConferenceInvite _ _ _ _ = return
  cConferenceMessage        :: Tox.ConferenceMessageCb        a
  cConferenceMessage _ _ _ _ _ = return
  cConferenceTitle          :: Tox.ConferenceTitleCb          a
  cConferenceTitle _ _ _ _ = return
  cConferencePeerName :: Tox.ConferencePeerNameCb a
  cConferencePeerName _ _ _ _ = return
  cConferencePeerListChanged :: Tox.ConferencePeerListChangedCb a
  cConferencePeerListChanged _ _ = return
  cFriendLossyPacket        :: Tox.FriendLossyPacketCb        a
  cFriendLossyPacket _ _ _ = return
  cFriendLosslessPacket     :: Tox.FriendLosslessPacketCb     a
  cFriendLosslessPacket _ _ _ = return


-- | Installs an event handler into the passed 'Tox' instance. After performing
-- the IO action, all event handlers are reset to null. This function does not
-- save the original event handlers.
withCHandler :: CHandler a => Tox a -> IO r -> IO r
withCHandler tox =
  install Tox.tox_callback_self_connection_status       (Tox.selfConnectionStatusCb      cSelfConnectionStatus     ) .
  install Tox.tox_callback_friend_name                  (Tox.friendNameCb                cFriendName               ) .
  install Tox.tox_callback_friend_status_message        (Tox.friendStatusMessageCb       cFriendStatusMessage      ) .
  install Tox.tox_callback_friend_status                (Tox.friendStatusCb              cFriendStatus             ) .
  install Tox.tox_callback_friend_connection_status     (Tox.friendConnectionStatusCb    cFriendConnectionStatus   ) .
  install Tox.tox_callback_friend_typing                (Tox.friendTypingCb              cFriendTyping             ) .
  install Tox.tox_callback_friend_read_receipt          (Tox.friendReadReceiptCb         cFriendReadReceipt        ) .
  install Tox.tox_callback_friend_request               (Tox.friendRequestCb             cFriendRequest            ) .
  install Tox.tox_callback_friend_message               (Tox.friendMessageCb             cFriendMessage            ) .
  install Tox.tox_callback_file_recv_control            (Tox.fileRecvControlCb           cFileRecvControl          ) .
  install Tox.tox_callback_file_chunk_request           (Tox.fileChunkRequestCb          cFileChunkRequest         ) .
  install Tox.tox_callback_file_recv                    (Tox.fileRecvCb                  cFileRecv                 ) .
  install Tox.tox_callback_file_recv_chunk              (Tox.fileRecvChunkCb             cFileRecvChunk            ) .
  install Tox.tox_callback_conference_invite            (Tox.conferenceInviteCb          cConferenceInvite         ) .
  install Tox.tox_callback_conference_message           (Tox.conferenceMessageCb         cConferenceMessage        ) .
  install Tox.tox_callback_conference_title             (Tox.conferenceTitleCb           cConferenceTitle          ) .
  install Tox.tox_callback_conference_peer_name         (Tox.conferencePeerNameCb        cConferencePeerName       ) .
  install Tox.tox_callback_conference_peer_list_changed (Tox.conferencePeerListChangedCb cConferencePeerListChanged ) .
  install Tox.tox_callback_friend_lossy_packet          (Tox.friendLossyPacketCb         cFriendLossyPacket        ) .
  install Tox.tox_callback_friend_lossless_packet       (Tox.friendLosslessPacketCb      cFriendLosslessPacket     )
  where
    install cInstall wrapper action =
      bracket wrapper (uninstall cInstall) $ \cb -> do
        () <- cInstall tox cb
        action

    uninstall cInstall cb = do
      freeHaskellFunPtr cb
      cInstall tox nullFunPtr
