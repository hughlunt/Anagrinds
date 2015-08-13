{-# LANGUAGE ForeignFunctionInterface #-}
module Anagrind where

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

-- impure function
foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
javacpp_init :: IO ()
javacpp_init = c_javacpp_init 0 nullPtr

-- pure function
foreign import ccall "anagrind" c_anagrind :: CString -> Bool
anagrind :: CString -> Bool
anagrind x = c_anagrind x


isAnagrind :: String -> IO Bool
isAnagrind x =
  do
    javacpp_init
    param <- newCString x
    return $ anagrind param
