module IOSpec
    ( spec
    ) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.IORef
import Data.Time.Clock          (DiffTime)
import System.Process
import Test.Hspec

import IO

spec :: Spec
spec = do
    describe "readLnMaybe" $ do
        it "parses an Int" $ testReadLnMaybe "42" `shouldReturn` Right "Just 42"
        it "returns Nothing if it can't parse an Int" $ testReadLnMaybe "I'm not an Int." `shouldReturn` Right "Nothing"
    describe "sumTwo" $ do
        it "works for 17 and 13" $ testSumTwo 17 13
        it "works for 1 and 99" $ testSumTwo 1 99
    describe "replicateM" $ do
        it "works with incrementing an IORef 100 times" $ testReplicateM 100
        it "works with incrementing an IORef zero times" $ testReplicateM 0
        it "works with incrementing an IORef (-42) times" $ testReplicateM (-42)

newtype Timeout = Timeout DiffTime deriving Show

instance Exception Timeout

diffTimeToMicroSeconds :: DiffTime -> Int
diffTimeToMicroSeconds t = ceiling $ 1e6 * toRational t

withTimeout :: DiffTime -> IO a -> IO (Either SomeException a)
withTimeout t x = do
    a <- async x
    b <- async $ threadDelay $ diffTimeToMicroSeconds t
    e <- waitEitherCatch a b
    return $ case e of
        Left (Left ex) -> Left ex
        Left (Right r) -> Right r
        Right _        -> Left $ toException $ Timeout t

cabalRun :: DiffTime -> String -> [String] -> IO (Either String [String])
cabalRun t n xs = do
    e <- withTimeout t $ readProcess "cabal" ["run", "-v0", n] (unlines xs)
    return $ case e of
        Left ex -> Left $ show ex
        Right s -> Right $ lines s

testReadLnMaybe :: String -> IO (Either String String)
testReadLnMaybe s = do
    e <- cabalRun 20 "readLnMaybe" [s]
    return $ case e of
        Left ex       -> Left ex
        Right []      -> Left "no output"
        Right (l : _) -> Right l

testSumTwo :: Int -> Int -> Expectation
testSumTwo m n =
    cabalRun 20 "sumTwo" (map show [m, n]) `shouldReturn` Right
        [ "Please enter first number:"
        , "Please enter second number:"
        , "The sum of both numbers is " ++ show (m + n) ++ "."
        ]

testReplicateM :: Int -> Expectation
testReplicateM n = go `shouldReturn` Right [1 .. n]
  where
    go :: IO (Either String [Int])
    go = fmap (either (Left . show) (Right . id)) $ withTimeout 5 $ do
        ref <- newIORef 0
        replicateM n $ do
            modifyIORef ref succ
            readIORef ref
