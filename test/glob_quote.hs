import HsShellScript

silly = "[[silly \\ filename]]"

main = do
   runprog "touch" ["/tmp/"++silly]
   
   q <- glob ("/tmp/"++glob_quote silly)
   outm ("Quoted: " ++ show q)

   nq <- glob ("/tmp/"++ silly)
   outm ("Non-quoted: " ++ show nq)
