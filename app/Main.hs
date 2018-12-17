module Main (main) where

import           Network.Wai.Handler.Warp  (run)

import           InterfaceAdaptor.InMemory (initialEnv)
import           InterfaceAdaptor.Server   (application)

main :: IO ()
main = run 8080 . application =<< initialEnv
