import Control.Concurrent.STM
import Control.Concurrent.Async
import System.IO


f :: TMVar () -> IO ()
f startbox = do
  putStrLn "   f BEGIN"
  atomically $ do
    --startBox <- takeTMVar startbox
    putTMVar startbox ()
  putStrLn "   f END"

f2 :: TMVar () -> IO ()
f2 startbox = do
  putStrLn "   f2 WAIT"
  atomically $ do
    startBox <- takeTMVar startbox
    putTMVar startbox ()
  putStrLn "   f2 END"



main :: IO ()

main = do startbox <- newEmptyTMVarIO
          putStrLn "MAIN BEGIN"
          concurrently_ (f startbox) (f2 startbox)
          putStrLn "MAIN END"
