to_ioe :: RunError -> IOError
to_ioe re =
   GHC.IO.Exception.IOError { ioe_handle      = Nothing,
                              ioe_type        = GHC.IO.Exception.SystemError,
                              ioe_location    = "runprog",
                              ioe_description = show_runerror re,
                              ioe_filename    = Just (shell_command (re_prog re) (re_pars re)),
                              ioe_errno       = re_errno re
                            }
      
{-
 FilePath -> IOErrorSource
                                
   GHC.IO.Exception.IOError { 
                              
                              
                              ioe_description = show_runerror re,
                              ioe_filename    = 
                              ioe_errno       = re_errno re
                            }
-}
{-
to_ioe re =
   System.IO.Error.ioeSetErrorString 
      ( System.IO.Error.mkIOError GHC.IO.Exception.SystemError       -- Type
                                  "runprog"                          -- Location
                                  Nothing                            -- handle 
                                  (Just (shell_command (re_prog re) (re_pars re)))   -- file name
      )
      (show_runerror re)
      { 
-}



-- In ProcErr.hs:317