{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Exception

import Data.Time.Clock
import Data.Time.Format

import System.Directory
import System.Process.Typed

rsnapshotSave :: [String] -> ProcessConfig () () ()
rsnapshotSave   = proc "rsnapshot" . (["-c", "/etc/rsnapshot-save.conf"] ++)

rsnapshot_save_0 :: FilePath
rsnapshot_save_0    = "/var/backups/rsnapshot/save/save.0"

doBackup :: MonadIO m => m Bool
doBackup    = liftIO $ do
    let day = fromInteger 86400
    b       <- doesDirectoryExist rsnapshot_save_0
    if b then (day <) <$> timeDiff
    else return True
  where
    --timeDiff :: MonadIO m => m NominalDiffTime
    timeDiff    = do
        now     <- getCurrentTime
        modt    <- getModificationTime rsnapshot_save_0
        return (now `diffUTCTime` modt)


runRsnapshot :: MonadIO m => m ()
runRsnapshot    = liftIO $ do
      runProcess_ (rsnapshotSave ["sync"])
      runProcess_ (rsnapshotSave ["save"])

main :: IO ()
main = do
    putStr "Saving root?.. "
    b <- doBackup
    if b
      then putStrLn "Yeah!" >> runRsnapshot >> putStrLn "Done."
      else putStrLn "No, let's do this another time."

