import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStr (welcome)
  repl 0

-- The main REPL loop
repl :: Int -> Nano.Env -> IO ()
repl n = do
  putStrFlush ("λ [" ++ show n ++ "] ")
  line <- getLine
  case (strCmd line) of
      CQuit -> doQuit
      CRun filepath -> do 
        doRun filepath
        repl (n + 1)  -- Keep the loop going!
      CLoad filepath -> do
        newEnv <- doLoad filepath  -- Capture the new environment!
        repl (n + 1)
      _       -> do
        doEval [("z1", Nano.VInt 0)] line
        repl (n + 1)


--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

