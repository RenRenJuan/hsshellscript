import HsShellScript

main = do
  outm "-1-"
  subproc ( exec "/bin/echo" ["bla"]
           -|= ( -- Do something with foo's output
                 do cnt <- lazy_contents "-"
                    outm ("in subprocess: read \"" ++ cnt ++ "\"")
               )
          )
  outm "-2-"
