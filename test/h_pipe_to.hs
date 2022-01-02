import System.Posix
import HsShellScript


main = mainwrapper $ do
  (h,pid) <- h_pipe_to (exec "/fooble" [])
  (Just ps) <- getProcessStatus True False pid
  print (ps::ProcessStatus)
  
  
{- 
     getProcessStatus block stopped pid
                      - Ob getProcessStatus blockieren soll
-}