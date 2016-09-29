{-# LANGUAGE Trustworthy #-}
module Network.Tox.C.Version where

import           Data.Word (Word32)

--------------------------------------------------------------------------------
--
-- :: API version
--
--------------------------------------------------------------------------------

-- | The major version number. Incremented when the API or ABI changes in an
-- incompatible way.
foreign import ccall tox_version_major :: Word32

-- | The minor version number. Incremented when functionality is added without
-- breaking the API or ABI. Set to 0 when the major version number is
-- incremented.
foreign import ccall tox_version_minor :: Word32

-- | The patch or revision number. Incremented when bugfixes are applied without
-- changing any functionality or API or ABI.
foreign import ccall tox_version_patch :: Word32

-- | Return whether the compiled library version is compatible with the passed
-- version numbers.
foreign import ccall tox_version_is_compatible :: Word32 -> Word32 -> Word32 -> Bool
