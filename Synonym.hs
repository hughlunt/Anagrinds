{-# LANGUAGE ForeignFunctionInterface #-}
module Synonym where

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

-- impure function
foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
javacpp_init :: IO ()
javacpp_init = c_javacpp_init 0 nullPtr

-- pure function
foreign import ccall "synonym" c_synonym :: CString -> CString -> Bool
synonym :: CString -> CString -> Bool
synonym ans def = c_synonym ans def


isSynonym :: String -> String -> IO Bool
isSynonym x y =
  do
    javacpp_init
    ans <- newCString x
    def <- newCString y
    return $ synonym ans def
