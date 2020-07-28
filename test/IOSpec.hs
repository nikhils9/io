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
        it "parses an Int" $ testReadLnMaybe "42" (Just 42)
        it "returns Nothing if it can't parse an Int" $ testReadLnMaybe "I'm not an Int." Nothing
    describe "sumTwo" $ do
        it "works for 17 and 13" $ testSumTwo 17 13
        it "works for 1 and 99" $ testSumTwo 1 99
    describe "replicateM" $ do
        it "works with incrementing an IORef 100 times" $ testReplicateM 100
        it "works with incrementing an IORef zero times" $ testReplicateM 0
        it "works with incrementing an IORef (-42) times" $ testReplicateM (-42)
    describe "sumMany" $ do
        it "should work with twenty numbers" $ testSumMany [1 .. 20]
        it "should work with zero numbers" $ testSumMany []
    describe "sumMany'" $ do
        it "should work with ten numbers" $ testSumMany' [1 .. 10]
        it "should work with zero numbers" $ testSumMany' []
    describe "wc" $
        it "should work with README.md" $ wc "README.md" `shouldReturn` (3, 16, 101)

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

testReadLnMaybe :: String -> Maybe Int -> Expectation
testReadLnMaybe s m = cabalRun 20 "readLnMaybe" [s] `shouldReturn` Right [show m]

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

testSumMany :: [Int] -> Expectation
testSumMany xs = go `shouldReturn` Right expected
  where
    n :: Int
    n = length xs

    inputs :: [String]
    inputs = show <$> n : xs

    go :: IO (Either String [String])
    go = cabalRun 20 "sumMany1" inputs

    expected :: [String]
    expected = "How many numbers do you want to add?" :
               replicate n "Enter next number:" ++
               ["The sum of all numbers is " ++ show (sum xs) ++ "."]

testSumMany' :: [Int] -> Expectation
testSumMany' xs = go `shouldReturn` Right expected
  where
    n :: Int
    n = length xs

    inputs :: [String]
    inputs = show <$> n : xs

    go :: IO (Either String [String])
    go = cabalRun 20 "sumMany2" inputs

    expected :: [String]
    expected = "How many numbers do you want to add?" :
               map (\i -> "Enter number " ++ show i ++ " of " ++ show n ++ ":") [1 .. n] ++
               ["The sum of all numbers is " ++ show (sum xs) ++ "."]

