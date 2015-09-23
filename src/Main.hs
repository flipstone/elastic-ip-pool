{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import            Control.Lens
import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Data.Maybe
import            Data.Foldable
import            Data.Text (Text)
import qualified  Data.Text as T
import qualified  Data.Text.IO as T
import qualified  Data.Text.Encoding as E
import            Network.AWS
import            Network.AWS.EC2.AssociateAddress
import            Network.AWS.EC2.DescribeAddresses
import            Network.AWS.EC2.Types
import            System.Environment
import            System.Exit
import            System.IO

type InstanceId = Text

data IP = IP {
    ipAddress :: Text
  , ipAllocationId :: Maybe Text
  , ipInstanceId :: Maybe InstanceId
  } deriving Show

newtype IPPool = IPPool {
    poolIPs :: [IP]
  } deriving Show

data Assignment =
    AlreadyAssigned InstanceId IP
  | ShouldAssign InstanceId IP
  | NotAvailable

instance Monoid Assignment where
  mempty = NotAvailable

  NotAvailable `mappend` a = a
  a `mappend` NotAvailable = a

  a@(AlreadyAssigned _ _) `mappend` _ = a
  _ `mappend` a@(AlreadyAssigned _ _) = a

  a@(ShouldAssign _ _) `mappend` _ = a

data Command =
    Show
  | TestAssign
  | Assign

extractIP :: Address -> Maybe IP
extractIP address = do
  publicAddr <- address ^. aPublicIP
  pure $ IP { ipAddress = publicAddr
            , ipAllocationId = address ^. aAllocationId
            , ipInstanceId = address ^. aInstanceId
            }

ipAssignment :: InstanceId -> IP -> Assignment
ipAssignment instanceId ip
  | ipInstanceId ip == Just instanceId = AlreadyAssigned instanceId ip
  | ipInstanceId ip == Nothing = ShouldAssign instanceId ip
  | otherwise = NotAvailable

assignIP :: InstanceId -> IPPool -> Assignment
assignIP instanceId = foldMap (ipAssignment instanceId) . poolIPs

main :: IO ()
main = do
  (command, addresses) <- loadArgs

  env <- newEnv NorthVirginia Discover

  runResourceT $ runAWS env $ do
    pool <- loadPool env addresses

    case command of
      Show -> showPool pool
      TestAssign -> testAssign pool
      Assign -> doAssign pool

loadArgs :: IO (Command, [Text])
loadArgs = do
  args <- getArgs

  let usage = do hPutStr stderr "Usage: elastic-ip-pool <show|test-assign|assign> ips..."
                 exitWith (ExitFailure 1)

  case args of
    [] -> usage
    [_] -> usage
    (commandArg:addressArgs) -> do
      command <- case commandArg of
                    "show" -> pure Show
                    "test-assign" -> pure TestAssign
                    "assign" -> pure Assign
                    _ -> usage

      pure (command, map T.pack addressArgs)


loadPool :: Env -> [Text] -> AWS IPPool
loadPool env addresses = do
  rs <- send describeAddresses

  let ips = mapMaybe extractIP $ rs ^. darsAddresses
      isInPool ip = ipAddress ip `elem` addresses
      poolIPs = filter isInPool ips

  pure $ IPPool poolIPs


showPool :: IPPool -> AWS ()
showPool pool = liftIO $
  for_ (poolIPs pool) $ \ip -> do
    T.putStr (ipAddress ip)
    T.putStr " - "
    T.putStr (fromMaybe "<unallocated>" $ ipInstanceId ip)
    T.putStr " - "
    T.putStr (fromMaybe "<classic>" $ ipAllocationId ip)
    T.putStr "\n"

testAssign :: IPPool -> AWS ()
testAssign pool = do
  assignment <- pickAssignment pool

  case assignment of
    ShouldAssign instanceId ip -> do
      (void $ send $ assignmentRequest instanceId ip
                   & aasDryRun .~ (Just True))
        `catch` \err ->
           case err of
             ServiceError serviceErr ->
               if (serviceErr ^. serviceCode == "DryRunOperation")
               then pure ()
               else throwM err
             _ -> throwM err

        `catch` \(err :: Error) -> liftIO $ do
           putStr "Test assignment failed with an error: "
           print err
           exitWith (ExitFailure 3)

      liftIO $ do
        putStr "Would have assigned "
        T.putStr (ipAddress ip)
        putStr " to "
        T.putStrLn instanceId

    NotAvailable -> liftIO $ do
      putStrLn "Not available ips found in the pool"

    AlreadyAssigned instanceId ip ->
      alreadyAssigned instanceId ip

doAssign :: IPPool -> AWS ()
doAssign pool = do
  assignment <- pickAssignment pool

  case assignment of
    ShouldAssign instanceId ip -> do
      rs <- send $ assignmentRequest instanceId ip

      liftIO $
        case rs ^. arsAssociationId of
          Just assocId -> do
            putStr "Associated "
            T.putStr (ipAddress ip)
            putStr " with "
            T.putStr instanceId
            putStr " (association id: "
            T.putStr assocId
            putStr ")\n"

          Nothing -> do
            putStr "Association call finished without exception, "
            putStr "but failed to associate an address. "
            putStr "The response code was: "
            print (show $ rs ^. arsResponseStatus)
            exitWith (ExitFailure 4)

    NotAvailable -> liftIO $ do
      T.putStrLn "No available ips found in the pool!"
      exitWith (ExitFailure 2)

    AlreadyAssigned instanceId ip ->
      alreadyAssigned instanceId ip

assignmentRequest :: InstanceId -> IP -> AssociateAddress
assignmentRequest instanceId ip =
    associateAddress
      & aasInstanceId .~ (Just instanceId)
      & aasAllocationId .~ allocation
      & aasPublicIP .~ publicIP
  where
    allocation = ipAllocationId ip
    publicIP = case allocation of
                 Just _ -> Nothing
                 Nothing -> Just $ ipAddress ip

alreadyAssigned :: InstanceId -> IP -> AWS ()
alreadyAssigned instanceId ip = liftIO $ do
  T.putStr instanceId
  T.putStr " is already assigned to "
  T.putStr $ ipAddress ip
  T.putStr "\n"

pickAssignment :: IPPool -> AWS Assignment
pickAssignment pool = do
  instanceId <- findInstanceId
  pure $ assignIP instanceId pool

findInstanceId :: AWS Text
findInstanceId = do
  onEC2 <- isEC2

  if onEC2
    then E.decodeUtf8 <$> metadata InstanceId
    else T.pack <$> (liftIO $ getEnv "EC2_INSTANCE_ID")

