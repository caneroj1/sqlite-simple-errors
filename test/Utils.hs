{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
(
  Test(..)
, executeTestRunner
)where

import Control.Monad.State
import Database.SQLite.Simple (open, close, execute_, Connection)
import SQLUtils
import System.Exit

data Test a = Test {
  testName   :: String
, expects    :: a
, testAction :: Connection -> IO a
}

data Results = Results {
  passed :: Int
, failed :: Int
}

instance Show Results where
  show Results{..} =
    "\nResults"  ++
    "\n-------"  ++
    "\npassed: " ++ show passed ++ "/" ++ show total ++
    "\nfailed: " ++ show failed ++ "/" ++ show total
    where total = passed + failed

empty :: Results
empty = Results 0 0

logResult :: Bool -> Results -> Results
logResult True  results = results{passed = passed results + 1}
logResult False results = results{failed = failed results + 1}

newtype TestRunner a = TestRunner {
  runSuite :: StateT Results IO a
} deriving (Monad, Functor, Applicative, MonadState Results, MonadIO)

executeTestRunner :: (Eq a, Show a) => [Test a] -> IO ()
executeTestRunner tests = do
  results <- execStateT (runSuite $ runTests tests) empty
  print results
  when (failed results > 0) exitFailure
  exitSuccess

setupDB :: Connection -> IO ()
setupDB conn = execute_ conn createTableSQL     >>
               execute_ conn createNextTableSQL >>
               execute_ conn "PRAGMA foreign_keys = ON"

runTests :: (Eq a, Show a) => [Test a] -> TestRunner ()
runTests []           = liftIO $ putStrLn "Done with tests"
runTests (test:tests) = do
  conn <- liftIO $ open ":memory:"
  liftIO $ setupDB conn
  got  <- liftIO $ testAction test conn
  liftIO $ close conn
  logResults got test
  runTests tests
  where
    logResults :: (Show a, Eq a) => a -> Test a -> TestRunner ()
    logResults got Test{..} = do
      liftIO $ do
        putStrLn   "\n"
        print      testName
        unless passed $ do
          putStr     "Expected: " >> print expects
          putStr     "Got:      " >> print got
        putStrLn   "----------"
        putStrLn $ "Passed:   " ++ show passed
      modify (logResult passed)
      where
        passed = expects == got
