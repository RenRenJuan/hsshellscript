import HsShellScript

main = do 
  putStrLn "-1-"
  runprog "/bin/echo" ["äöü"] ->- "/tmp/redirect"
  putStrLn "-2-"
