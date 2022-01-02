import Foreign.C
import Foreign.Ptr


main = 
  aufruf "äöü"


aufruf :: String -> IO ()
aufruf str =
  withCString str $ \ptr -> c_aufruf ptr


foreign import ccall safe "test/glob.chs.h c_aufruf"
  c_aufruf :: ((Ptr CChar) -> (IO ()))

