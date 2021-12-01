{-# LANGUAGE Safe       #-}
{-# LANGUAGE StrictData #-}
module Network.Tox.C.Type where

import           Control.Concurrent.MVar (MVar)
import           Foreign.Ptr             (Ptr)
import           Foreign.StablePtr       (StablePtr)


-- | The Tox instance type. All the state associated with a connection is held
-- within the instance. Multiple instances can exist and operate concurrently.
-- The maximum number of Tox instances that can exist on a single network device
-- is limited. Note that this is not just a per-process limit, since the
-- limiting factor is the number of usable ports on a device.
data ToxStruct a
type Tox a = Ptr (ToxStruct a)

type UserData a = StablePtr (MVar a)
