import HsShellScript

main = do
   erg <- fdupes ["-r", "-s", "-H"] ["."]
   print erg

