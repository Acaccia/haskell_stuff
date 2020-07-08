import Control.Concurrent.Async
import Control.Concurrent.STM


f :: TMVar () -> IO ()
f startbox
  = do putStrLn "   f BEGIN"
       atomically $ putTMVar startbox ()
       putStrLn "   f END"


f2 :: TMVar () -> IO ()
f2 startbox
  = do putStrLn "   f2 WAIT"
       atomically $ takeTMVar startbox
       putStrLn "   f2 END"



main :: IO ()
main = do putStrLn "BEGIN"
          startbox <- newEmptyTMVarIO
          concurrently_ (f startbox) (f2 startbox)
          putStrLn "END"
