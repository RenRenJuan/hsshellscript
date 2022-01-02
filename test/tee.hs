import HsShellScript

main = do
   outm "Davor."
   subproc
     ( (err_to_out main')
       -|- exec "/usr/bin/tee" ["/tmp/tee.log"] )
   outm "Danach."

main' =
     outm "In main'"
