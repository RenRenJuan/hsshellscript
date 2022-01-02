import Control.Exception
import Prelude hiding (catch)
import HsShellScript

main = mainwrapper $ do
   outm "RunError:"
   runprog "/bin/false" []
     `catch` (\(re::RunError) -> do
                outm "\nshow re:"
                outm (show re)
                outm "\nshow_runerror:"
                outm (show_runerror re)
             )
   
   outm "\n\nIOError:" 
   (as_ioe $ runprog "/bin/false" [])
     `catch` (\(ioe::IOError) -> 
                 outm (show ioe))

   outm "\n\nEnd."
