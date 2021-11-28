module Data.ByteString.Arbitrary
( ArbByteString(..)
, ArbByteString1M(..)
, ArbByteString10M(..)
, fastRandBs
, slowRandBs
) where

import           Crypto.Hash.Skein512 (hash)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Test.QuickCheck      (Arbitrary (..), Gen, choose, vectorOf)

-- | A ByteString wrapper so we can implement Arbitrary for ByteString. This
-- will currently generate random ByteStrings of length 0 to 100KB.
newtype ArbByteString = ABS { fromABS :: ByteString }
  deriving (Eq, Ord, Read, Show )

-- | A wrapper to generate 1MB bytestrings. The shrink implementation still
-- returns "ArbByteString1M" instances, of course, but they're smaller than
-- 1MB.
newtype ArbByteString1M = ABS1M { fromABS1M :: ByteString }
  deriving (Eq, Ord, Read, Show )

-- | A wrapper to generate 10MB bytestrings. I should really figure out how
-- type-level Nats work, so one can just do (ArbByteStringN 10000000) and have
-- selectable sizes, but I don't see how to do that yet, so 10MB is as big as
-- this library goes. As with the 1MB version, shrink here will generate
-- ArbByteString10M instances that wrap ByteStrings smaller than 10MB.
newtype ArbByteString10M = ABS10M { fromABS10M :: ByteString }
  deriving (Eq, Ord, Read, Show )

instance Arbitrary ArbByteString where
  arbitrary = do
    len <- choose (0, 100*1024)
    ABS `fmap` fastRandBs len

  shrink (ABS bs) = map ABS $ shrinks bs

instance Arbitrary ArbByteString1M where
  arbitrary =
    ABS1M `fmap` fastRandBs (1024*1024)

  shrink (ABS1M bs) = map ABS1M $ shrinks bs

instance Arbitrary ArbByteString10M where
  arbitrary =
    ABS10M `fmap` fastRandBs (10*1024*1024)

  shrink (ABS10M bs) = map ABS10M $ shrinks bs

-- | Generate a bunch of binary data quickly. This abuses the cryptohash skein
-- function to generate a megabyte of data at a time, and then concats chunks
-- until it has enough.
fastRandBs :: Int -> Gen ByteString
fastRandBs len = do
  let perChunk = 1024*1024
  let (rounds, bytes) = len `divMod` perChunk
  bSeed <- slowRandBs $ 16 -- 16 bytes of "really" random seed

  -- Notice the hash (8*) calls; hash always returns an integral number of
  -- bytes (duh), but it wants its output length in bits. We just always track
  -- bytes, and multiply by 8 when calling hash.
  let preChunks = if bytes == 0 then BS.empty else hash (8*bytes) bSeed
  if rounds == 0
    then return preChunks
    else do
      rSeed <- slowRandBs $ 16
      let hashes = tail $ iterate ( hash (8*perChunk) . BS.take 32 ) rSeed
      return $ BS.concat $ preChunks : take rounds hashes

-- | Generate binary data slowly. This generates a list of Word8s, and then
-- uses Data.ByteString.pack to concatenate it into a single ByteString.
slowRandBs :: Int -> Gen ByteString
slowRandBs numBytes = BS.pack `fmap` vectorOf numBytes (choose (0, 255))

shrinks :: ByteString -> [ByteString]
shrinks bs =
  [ BS.append a b | (a, b) <- zip (BS.inits bs) (tail $ BS.tails bs) ]
