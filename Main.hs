{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, QuasiQuotes #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.Console.CmdArgs hiding (opt)
import Data.Typeable ()
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative
import Safe (headMay)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import System.IO (stdout)
import System.Process.QQ

import AWS
import AWS.EC2 (EC2)
import qualified AWS.EC2 as EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as EU
import qualified AWS.EC2.Metadata as MD

data Option = Option
    { optVolumeSize :: Int
    , optDeviceName :: String
    , optSrcDir :: String
    , optFilesystem :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { optVolumeSize = 10
      &= name "size"
      &= explicit
      &= help "Volume size"
    , optDeviceName = "/dev/sdp"
      &= name "device"
      &= explicit
      &= help "Device name"
    , optSrcDir = "/"
      &= name "srcdir"
      &= explicit
      &= help "Source directory"
    , optFilesystem = "ext3"
      &= name "filesystem"
      &= explicit
      &= help "Filesystem"
    }
    &= summary "Extend size of EBS volume"
    &= program "volext"

createNewVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Int -- ^ Size
    -> EC2 m Volume
createNewVolume size = do
    az <- liftIO $ MD.availabilityZone
    EC2.setRegion $ T.init az
    vol <- EC2.createVolume (CreateNewVolume size az Nothing)
    waitForVolume VolAvailable $ volumeId vol

waitForVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => VolumeStatus
    -> Text
    -> EC2 m Volume
waitForVolume st = EU.wait
    (\v -> st == volStatus v)
    (\i -> EU.asList $ EC2.describeVolumes [i] [])

attachNewVolume
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VolumeId
    -> Text -- ^ DeviceName
    -> EC2 m Volume
attachNewVolume vid devname = do
    iid <- liftIO $ MD.instanceId
    EC2.attachVolume vid iid devname
    waitForAttachment AttAttached vid

waitForAttachment
    :: (MonadResource m, MonadBaseControl IO m)
    => AttachmentStatus
    -> Text
    -> EC2 m Volume
waitForAttachment st = EU.wait
    (\v -> Just st == (attStatus <$> headMay (volAttachmentSet v)))
    (\i -> EU.asList $ EC2.describeVolumes [i] [])

format
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ fs name
    -> Text -- ^ device name
    -> m ()
format fs dev = do
    liftIO $ putStrLn "format.."
    [scmd|/sbin/mkfs.#{fs} #{dev}|] $$ CB.sinkHandle stdout
  where

dataCopy
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ device
    -> Text -- ^ source directory
    -> m ()
dataCopy dev srcDir = do
    liftIO $ putStrLn "data copy.."
    [scmd|mount #{dev} /mnt/ext|]
    >> [scmd|rsync -a -delete -progress -x #{srcDir} /mnt/ext|]
    >> [scmd|umount /mnt/ext|]
    $$ CB.sinkHandle stdout

main :: IO ()
main = do
    opt <- cmdArgs option
    let size = optVolumeSize opt
    let devName = T.pack $ optDeviceName opt
    let fs = T.pack $ optFilesystem opt
    let srcDir = T.pack $ optSrcDir opt
    putStrLn $ "size: " ++ show size
    putStrLn $ "device: " ++ T.unpack devName
    putStrLn $ "fs: " ++ T.unpack fs
    cred <- loadCredential
    runResourceT $ EC2.runEC2 cred $ do
        vol <- createNewVolume size
        let vid = volumeId vol
        liftIO $ putStr "create volume: "
        liftIO $ putStrLn $ T.unpack vid
        attachNewVolume vid devName
        liftIO $ putStrLn "attached volume."

        lift $ format fs devName
        lift $ dataCopy devName srcDir

        EC2.detachVolume vid Nothing Nothing Nothing
        waitForVolume VolAvailable vid
        liftIO $ putStrLn "complete."
