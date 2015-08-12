{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

-- impure function
foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
javacpp_init :: IO ()
javacpp_init = c_javacpp_init 0 nullPtr

-- pure function
foreign import ccall "anagrind" c_anagrind :: CString -> Bool
anagrind :: CString -> Bool
anagrind a = c_anagrind a

-- isAnagrind = do
--    javacpp_init
--    param <- newCString "wrong"
--    print $ anagrind param
--
-- main = do
--  isAnagrind

isAnagrind :: String -> IO Bool
isAnagrind x =
  do
    javacpp_init
    param <- newCString x
    return $ anagrind param
    -- success <- anagrind param


main = do
  -- javacpp_init
  success <- isAnagrind "battle"
  if success then print True else print False
