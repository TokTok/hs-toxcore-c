module Network.Tox.C.Callbacks where

import           Control.Exception  (bracket)
import           Foreign.Ptr        (freeHaskellFunPtr, nullFunPtr)

import qualified Network.Tox.C.Tox  as Tox
import           Network.Tox.C.Type (Tox)


-- | Low level event handler. The functions in this class are directly
-- registered with the corresponding C callback. We use 'StablePtr' to pass
-- opaque Haskell values around in C.
class CHandler a where
  cSelfConnectionStatus   :: Tox.SelfConnectionStatusCb   a
  cFriendName             :: Tox.FriendNameCb             a
  cFriendStatusMessage    :: Tox.FriendStatusMessageCb    a
  cFriendStatus           :: Tox.FriendStatusCb           a
  cFriendConnectionStatus :: Tox.FriendConnectionStatusCb a
  cFriendTyping           :: Tox.FriendTypingCb           a
  cFriendReadReceipt      :: Tox.FriendReadReceiptCb      a
  cFriendRequest          :: Tox.FriendRequestCb          a
  cFriendMessage          :: Tox.FriendMessageCb          a
  cFileRecvControl        :: Tox.FileRecvControlCb        a
  cFileChunkRequest       :: Tox.FileChunkRequestCb       a
  cFileRecv               :: Tox.FileRecvCb               a
  cFileRecvChunk          :: Tox.FileRecvChunkCb          a
  cFriendLossyPacket      :: Tox.FriendLossyPacketCb      a
  cFriendLosslessPacket   :: Tox.FriendLosslessPacketCb   a


-- | Installs an event handler into the passed 'Tox' instance. After performing
-- the IO action, all event handlers are reset to null. This function does not
-- save the original event handlers.
withCHandler :: CHandler a => Tox a -> IO r -> IO r
withCHandler tox =
  install Tox.tox_callback_self_connection_status   (Tox.selfConnectionStatusCb   cSelfConnectionStatus  ) .
  install Tox.tox_callback_friend_name              (Tox.friendNameCb             cFriendName            ) .
  install Tox.tox_callback_friend_status_message    (Tox.wrapFriendStatusMessageCb    cFriendStatusMessage   ) .
  install Tox.tox_callback_friend_status            (Tox.wrapFriendStatusCb           cFriendStatus          ) .
  install Tox.tox_callback_friend_connection_status (Tox.wrapFriendConnectionStatusCb cFriendConnectionStatus) .
  install Tox.tox_callback_friend_typing            (Tox.wrapFriendTypingCb           cFriendTyping          ) .
  install Tox.tox_callback_friend_read_receipt      (Tox.wrapFriendReadReceiptCb      cFriendReadReceipt     ) .
  install Tox.tox_callback_friend_request           (Tox.wrapFriendRequestCb          cFriendRequest         ) .
  install Tox.tox_callback_friend_message           (Tox.wrapFriendMessageCb          cFriendMessage         ) .
  install Tox.tox_callback_file_recv_control        (Tox.wrapFileRecvControlCb        cFileRecvControl       ) .
  install Tox.tox_callback_file_chunk_request       (Tox.wrapFileChunkRequestCb       cFileChunkRequest      ) .
  install Tox.tox_callback_file_recv                (Tox.wrapFileRecvCb               cFileRecv              ) .
  install Tox.tox_callback_file_recv_chunk          (Tox.wrapFileRecvChunkCb          cFileRecvChunk         ) .
  install Tox.tox_callback_friend_lossy_packet      (Tox.wrapFriendLossyPacketCb      cFriendLossyPacket     ) .
  install Tox.tox_callback_friend_lossless_packet   (Tox.wrapFriendLosslessPacketCb   cFriendLosslessPacket  )
  where
    install cInstall wrapper action =
      bracket wrapper (uninstall cInstall) $ \cb -> do
        () <- cInstall tox cb
        action

    uninstall cInstall cb = do
      freeHaskellFunPtr cb
      cInstall tox nullFunPtr
