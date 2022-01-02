import HsShellScript
import Control.Exception
import Prelude hiding (catch)

main = do
   outm "run:"
   (mainwrapper $ run "/frooble" [])
      `catch` (\(e::SomeException) -> print e)

   outm "\n\nrunprog:"
   (mainwrapper $ runprog "/frooble" [])
      `catch` (\(e::SomeException) -> print e)
