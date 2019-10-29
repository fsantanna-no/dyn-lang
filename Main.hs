import Test.HUnit
import System.IO (hPutStrLn, stderr)
import System.Directory
import System.Process
import System.Exit
import Debug.Trace

import Dyn.Eval

main :: IO ()
main = do
    src <- readFile "/tmp/x.dis"
    putStrLn $ evalString False src
    return ()
