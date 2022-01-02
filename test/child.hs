import HsShellScript


main = mainwrapper $ do
   pipe_to "bla" (exec "/bin/false" [])
