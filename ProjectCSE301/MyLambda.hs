import Expr
import Cmd
import Subst
import Eval
import PrettyExpr
import Parser
import Data.List
import System.IO
import Data.Maybe


-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var,LExp)]

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> lookup y env == Nothing) (free t)



-- the top-level read-eval-print-loop
repl :: IO ()
repl = go []                     -- start the interpreter in an empty environment
  where
    go :: Env -> IO ()
    go env = do
      putStr "> "                -- print the prompt
      hFlush stdout              -- flush standard output
      
      input <- getLine           -- read a line of input

      let cmd = (runParser parseCmd) input
      case cmd of
        Nothing -> putStrLn "parse error" >> go env
        
        otherwise -> let cmdf = fst (fromJust cmd) in   
          case cmdf of                
              Eval t ->              -- execute an eval command
                -- the expression to be evaluated cannot have any free
                -- variables that are not defined in the environment
                case undefinedVar env t of
                  Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
                  Nothing -> do
                    -- substitute for all variables defined in the environment,
                    -- in order from left to right
                    let t' = foldl (\t (x,u) -> subst (u,x) t) t env
                    -- normalize the resulting term
                    let u = normalize t'
                    -- print the result
                    printLExp u
                    -- continue the REPL
                    go env
                    
              Let x t ->             -- execute a let command
                case undefinedVar env t of
                  Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
                  Nothing -> do
                    -- continue the REPL in an environment extended with x=t
                    go ((x,t):env)
                  
              Noop -> go env         -- execute a no-op command, by doing nothing and continuing the REPL

              Quit -> do             -- execute a quit command, by terminating the REPL
                print "Goodbye."
                return ()
              

                
            

              -- in case load is called, we read the file and parse it
              

main = repl