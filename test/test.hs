
import HsShellScript

main = do
  outm "-1-"
  subproc (outm "Durch die Röhre" -|- exec "/bin/cat" [])
  outm "-2-"

  mount <- pipe_from (exec "/bin/mount" [])
  outm "Mount:"
  outm mount
