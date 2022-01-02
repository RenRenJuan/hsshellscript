import HsShellScript
import Control.Exception
import Data.Typeable

data Ausnahme = Ausnahme
   deriving (Show, Typeable)

instance Exception Ausnahme


main = mainwrapper $ do
   subproc (throw Ausnahme)
