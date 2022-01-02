import HsShellScript
import System.IO

main = mainwrapper $ do

   hSetBuffering stdout (BlockBuffering (Just 10000))
   hSetBuffering stderr (BlockBuffering (Just 10000))

   erg <- pipe_from $
         exec "/schwurbel" []

   putStrLn erg
