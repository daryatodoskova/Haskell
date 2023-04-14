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

parse :: String -> Maybe (Cmd,String)
parse s = runParser parseCmd s

parse' :: String -> Maybe Cmd
parse' s = case parse s of
             Nothing -> Nothing
             Just (c,_) -> Just c

parse'' :: String -> Cmd
parse'' s = case parse' s of
              Nothing -> error "parse error"
              Just c -> c

-- the top-level read-eval-print-loop
repl :: IO ()
repl = go []                     -- start the interpreter in an empty environment
  where
    go :: Env -> IO ()
    go env = do
      putStr "> "                -- print the prompt
      hFlush stdout              -- flush standard output
      
      input <- getLine           -- read a line of input

      let cmd = (runParser parseCmd input)  
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
              
              -- load lines from file, parse them, and execute them
              Load filename -> do
                handle <- openFile filename ReadMode
                contents <- hGetContents handle
                let cmds = map parse'' (lines contents)
                let cmds' = map (\cmd -> case cmd of
                                            Eval t -> Eval t
                                            Let x t -> Let x t
                                            Noop -> Noop
                                            Quit -> Quit
                                            Load filename -> Load filename) cmds
                -- execute each command
                let env' = foldl (\env cmd -> case cmd of
                                                Eval t -> env
                                                Let x t -> (x,t):env
                                                Noop -> env
                                                Quit -> env
                                                Load filename -> env) env cmds'
                -- continue the REPL
                go env'
                -- close the file
                hClose handle

main = repl


-- tests
{-
> zero = \f.\x.x
> one = \f.\x.f x
> succ = \n.\f.\x.f (n f x)
> two = succ one
> two
\f.\x.f (f x)
> add = \m.\n.m succ n
> mul = \m.\n.m (add n) zero
> exp = \m.\n.n (mul m) one
> add one one
\f.\x.f (f x)
> add one one
\f.\x.f (f x)
> three = succ one
> three = succ two
> three
\f.\x.f (f (f x))
> add one (add one one)
\f.\x.f (f (f x))
> add one (mul one two)
\f.\x.f (f (f x))
> add one two
\f.\x.f (f (f x))
> add two one
\f.\x.f (f (f x))
> and one zero
\x.x
> mul one two
\f.\x.f (f x)
> mul one three
\f.\x.f (f (f x))
> add two three
\f.\x.f (f (f (f (f x))))
> exp two three
\f.\x.f (f (f (f (f (f (f (f x)))))))
> mul two three
\f.\x.f (f (f (f (f (f x)))))
> :quit
"Goodbye."-}

{-
> true = \x.\y.x
> false = \x.\y.y
> not = \b.b false true
> and = \b1.\b2.b1 b2 false
> or = \b1.\b2.b1 true b2
> or true false
\x.\y.x
> or false true
\x.\y.x
> not true
\x.\y.y
> not false
\x.\y.x
> and false true
\x.\y.y
> and true true
\x.\y.x
> not and true true
\x.\y.x
> not (and true true)
\x.\y.y
> or (and false false) (or false true)
\x.\y.x
> or (and false false) (or true true)
\x.\y.x
-}