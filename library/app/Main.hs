module Main (main) where

import InterfaceAdaptor.InMemory (initialEnv)
import InterfaceAdaptor.Server (application)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 . application =<< initialEnv
