{-# LANGUAGE BangPatterns #-}

module IOSpec
    ( spec
    ) where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception
import           Data.IORef
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Time.Clock                  (DiffTime)
import           Statistics.Distribution
import           Statistics.Distribution.Binomial (binomial)
import           System.Process
import           Test.Hspec

import           IO

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
    describe "testTwoDice" $
        it "should give the expected distribution for 1000000 tosses" $ testTwoDice 1000000 1e-7
    describe "httpTest" $
        it "should return an HTTP 200" $ testHttp httpTest
    describe "httpTest'" $
        it "should return an HTTP 200" $ testHttp httpTest'
    describe "dnsTest" $
        it "should return at least on IP address" testDns

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

getExpectedInterval :: Int -> Double -> (Int, Int)
getExpectedInterval n e = (lowerBound, upperBound)
  where
    d = binomial n $ 1 / 36
    e2 = e / 2

    lowerBound :: Int
    lowerBound = go 0 0
      where
        go s i =
            let p   = probability d i
                !s' = s + p
            in  if s' > e2 then i else go s' $ succ i

    upperBound :: Int
    upperBound = go 0 n
      where
        go s i =
            let p   = probability d i
                !s' = s + p
            in  if s' > e2 then i else go s' $ pred i

histogram :: Int -> IO [Int]
histogram n = go M.empty n
  where
    go :: Map (Int, Int) Int -> Int -> IO [Int]
    go m 0 = return $ (\xy -> M.findWithDefault 0 xy m) <$> [(x, y) | x <- [1..6], y <- [1..6]]
    go !m !i = do
        xy <- twoDice
        let m' = M.alter (maybe (Just 1) (Just . succ)) xy m
        go m' $ pred i

testTwoDice :: Int -> Double -> Expectation
testTwoDice n e = go `shouldReturn` True
  where
    go :: IO Bool
    go = do
        h <- histogram n
        let (l, u) = getExpectedInterval n e
        return $ all (\i -> i >= l && i <= u) h

testHttp :: IO [String] -> Expectation
testHttp a = head <$> a `shouldReturn` "HTTP/1.1 200 OK\r"

testDns :: Expectation
testDns = go `shouldReturn` True
  where
    go :: IO Bool
    go = either (const False) (not . null) <$> dnsTest
