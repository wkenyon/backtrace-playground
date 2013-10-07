module Main where

import Parser
import Syntax
import Lexer
import Eval
import EvalTypes (topStack, cafStack)
--import qualified Eval2
--import qualified Eval3
--import qualified Eval4
-- import EvalDebug2
--import EvalTypes

--import qualified Oper1
--imort qualified Oper2
--import qualified Oper3

import Control.Applicative
import System.Environment
import Text.PrettyPrint
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit
import Text.Printf

main = do
  [ file ] <- getArgs
  run file  

run :: FilePath -> IO ()
run file = do
  line
  putStrLn ("test: " ++ file)
  line
  s <- readFile file
  let prog1 = parse $ alexScanTokens s
  
  putStrLn "prog1:"
  putStrLn (render (pprProg1 prog1))


  let (prog, startUniq) = runState (starProg prog1) 1
  putStrLn "prog2:"
  putStrLn (render (pprProg prog))

  let 
        init_heap cs  = Map.fromList [ (x, (cs,e)) | (x,e) <- prog ]
	top_expr = ELet ("x", EInt 0) (EApp (EVar "main") "x")
        
        (((_,result), (_,_)), _) =
                runWriter $ runStateT (eval topStack top_expr) (init_heap cafStack,startUniq)


{-        ((oper1_result, (_, oper1_costs,_)), oper1_msgs) =
                runWriter $ runStateT (Oper1.eval topStack top_expr) (init_heap,emptyCosts,1)

        ((oper2_result, (_, oper2_costs,_)), oper2_msgs) =
                runWriter $ runStateT (Oper2.eval topStack top_expr) (init_heap,emptyCosts,1)

        ((oper3_result, (_, oper3_costs,_)), oper3_msgs) =
                runWriter $ runStateT (Oper2.eval topStack top_expr) (init_heap,emptyCosts,1)
-}  
-- in
  putStr "\n"
  putStrLn "result (eval):"
  putStrLn (render (pprExpr result))


{-  when (oper1_result /= result) $ do
	putStrLn ("*** ERROR: oper1 result wrong: " ++ 
		render (pprExpr oper1_result))
	exitWith (ExitFailure 1)
  when (oper2_result /= result) $ do
	putStrLn ("*** ERROR: oper2 result wrong: " ++ 
		render (pprExpr oper2_result))
	exitWith (ExitFailure 1)
  when (oper3_result /= result) $ do
	putStrLn ("*** ERROR: oper3 result wrong: " ++ 
		render (pprExpr oper3_result))
	exitWith (ExitFailure 1)
  when (oper1_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper1 costs wrong: " ++ showCosts oper1_costs)
	exitWith (ExitFailure 1)
  when (oper2_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper2 costs wrong: " ++ showCosts oper2_costs)
	exitWith (ExitFailure 1)
  when (oper3_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper3 costs wrong: " ++ showCosts oper3_costs)
	exitWith (ExitFailure 1)
-}  
  putStr "\n\n"

line = putStrLn "-------------------------------------"
