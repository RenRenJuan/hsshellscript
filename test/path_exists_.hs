import HsShellScript
import System.Posix.Files

main = do 
  createSymbolicLink "/frooble" "symlink"
  
  outm_ "Symlink exists: "
  ex <- path_exists' "symlink"
  outm (show ex)
  
  outm_ "Path exists: "
  ex <- path_exists "symlink"
  outm (show ex)

  removeLink "symlink"
  