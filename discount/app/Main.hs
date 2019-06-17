module Main (main) where

import           CLI                     (cli)
import           RateRepositoryImplement (MockRate (runMockRate))

main :: IO ()
main = runMockRate cli
