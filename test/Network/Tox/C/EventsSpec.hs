{-# LANGUAGE StrictData   #-}
{-# LANGUAGE Trustworthy  #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Tox.C.EventsSpec where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM)
import           Data.List            (sort)
import           Data.MessagePack     (Object (..))
import qualified Data.MessagePack     as MP
import qualified Data.Vector          as V
import           Test.Hspec
import           Test.QuickCheck

import qualified Network.Tox.C        as C
import           Network.Tox.C.Events


getRight :: (MonadFail m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r


must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


processEvent :: Event -> IO Bool
processEvent SelfConnectionStatus{} = return True
processEvent _                      = return False


toxIterate :: Int -> [C.Tox] -> IO ()
toxIterate 0 _ = expectationFailure "could not bootstrap"
toxIterate countdown toxes = do
    interval <- foldr max 0 <$> mapM C.toxIterationInterval toxes
    threadDelay $ fromIntegral $ interval * 10000

    connected <- fmap and . forM toxes $ \tox -> do
        events <- must $ C.toxEventsIterate tox
        or <$> mapM processEvent events

    if connected
       then putStrLn "connected"
       else toxIterate (countdown - 1) toxes


spec :: Spec
spec = do
    describe "event serialisation format" $ do
        it "is linear encoding" $
            MP.toObject MP.defaultConfig (FileChunkRequest 1 2 3 4)
                `shouldBe` ObjectArray (V.fromList
                    [ ObjectWord 11
                    , ObjectArray (V.fromList
                        [ObjectWord 1,ObjectWord 2,ObjectWord 3,ObjectWord 4])])

        it "can round-trip through toxcore" $
            property $ \(sort -> events) -> do
                events' <- C.toxEventsToPtr events >>= C.toxEventsFromPtr
                sort <$> events' `shouldBe` Right events

    describe "toxcore" $
        it "can bootstrap" $ do
            must $ C.withTox C.defaultOptions $ \tox1 ->
                must $ C.withTox C.defaultOptions $ \tox2 -> do
                    bootstrapPort <- must $ C.toxSelfGetUdpPort tox1
                    bootstrapKey <- C.toxSelfGetDhtId tox1
                    must $ C.toxBootstrap tox2 "localhost" bootstrapPort bootstrapKey

                    toxIterate 100 [tox1, tox2]
