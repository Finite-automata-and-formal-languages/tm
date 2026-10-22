module Main where

import Control.Monad
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Data.Text qualified as T

import TM

foreign import javascript unsafe "(f => check_tm = f)"
  setCheckTm :: Callback a -> IO ()

main = do
  setCheckTm <=< syncCallback3' $ \ a b c ->
    jsNull <$ checkTm (fromJSString a == "valid") (fromJSString b) (T.pack (fromJSString c))
