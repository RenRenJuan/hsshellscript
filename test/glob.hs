import HsShellScript

main = do
   l <- glob "*"
   mapM_ outm l
   