{-# OPTIONS -fglasgow-exts #-}
module Eval2 where

import Syntax
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import Control.Monad.State
import Data.Map (Map)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Multi
import qualified Data.Map as Map
import Text.PrettyPrint
import Data.Maybe
import qualified Debug.Trace as Trace
-- -----------------------------------------------------------------------------
type FreshID=Int
type E a = StateT (Heap,FreshID) (Writer [String]) a

-----------------------------------------------------------------------------

type Heap = Map Var (Stack,Expr)

-----------------------------------------------------------------------------

type Stack = [Label]

topStack = ["TOPSTACK"]

cafLabel  = "CAF"
cafStack = [cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'

showCCS = render . pprStack
----------------------------------------------------------------------------


trace :: String -> E ()
trace s = Trace.trace s $ W.tell [s]
-- -----------------------------------------------------------------------------
-- The evaluator

-- This is a rendering of the lazy evaluation "natural semantics"
-- using a state monad to keep track of the heap.  It is very similar
-- to the cost centre semantics in Sansom's thesis (p. 47), the
-- differences being:
--
-- 	* pass/return cost centre *stacks*
--
--	* the SUB function in the variable rule has been replaced by
--	  funCall for lambdas, and for thunks we always set the Stack
--	  for evaluation.  We never subsume the costs of a thunk-
--	  this was implicit in Sansom's semantics because there were
--	  never any thunks with a "SUB" cost centre, but it is explicit
--	  in ours).
--
--	* The scc rule *pushes* the Label on the current Stack.

eval :: Stack -> Expr -> E (Stack,Expr)
eval ccs (EVar x) = eval_var ccs x 
eval ccs (EInt i) = return (ccs,EInt i)
eval ccs (ELam x e) = return (ccs,ELam x e)
eval ccs (ECons cs) = return (ccs,ECons cs)
eval ccs (ELet (x,e1) e2) = modifyHeap (\h -> Map.insert x (ccs,e1) h) >> eval ccs e2
eval ccs (EPlus e1 e2) = do
   (_,EInt x) <- eval ccs e1
   (_,EInt y) <- eval ccs e2
   return (ccs,EInt (x+y))

eval ccs (ECase e alts) = do
   (_,ECons (name,vars)) <- eval ccs e
   let (vars',e') = fromJust $ lookup name $ map (\((name,vars),e)->(name,(vars,e))) alts
   eval ccs $ substList vars' vars e'
   

eval ccs (EApp f x) = do
   (lam_ccs, ELam y e) <- eval ccs f
   eval lam_ccs (subst y x e)
   

eval ccs (EPush cc e) = 
   eval (pushCC cc ccs) e
  
eval ccs (EBreak e) = do
   trace $ ('\n':showCCS ccs)
   eval ccs e

eval_var ccs x = do
   r <- lookupHeap x
   case r of
	(ccs', EInt i) -> do
	   return (ccs',EInt i)
        (ccs', ECons cs) -> do
           e' <- eHat $ ECons cs
           return (ccs',e') 
	(ccsv, ELam y e) -> do
           e' <- eHat $ ELam y e
	   enter ccs ccsv x e'

	(ccs',e) -> do 
           modifyHeap (\h -> Map.delete x h)
		-- delete it from the heap so we can get blackholes
           (ccsv, v) <- eval ccs' e
           update x (ccsv,v)
           v' <- eHat v 
	   enter ccs ccsv x v'

enter ccs ccsv x (EInt i)   = return (ccs,EInt i)
enter ccs ccsv x (ECons cs) = return (ccs,ECons cs)
enter ccs ccsv x (ELam y e) = do
  let call_ccs = funCall ccs ccsv
  return (call_ccs,ELam y e)
enter ccs ccsv x e = error $ show $ text "Match Error:" <+> pprExpr e

-- -----------------------------------------------------------------------------
-- Stack operations
pushCC :: Label -> Stack -> Stack
pushCC = (:)

funCall :: Stack -> Stack -> Stack
funCall = swap (++)
swap f x y = f y x
--  | not (null lam_ccs) && last lam_ccs == "CAF"  
--  = {- trace ("funCall, ccs = " ++ show ccs) $ -} ccs `appendCCS` lam_ccs
--  | otherwise
--  = {- trace ("funCall, lam_ccs = " ++ show lam_ccs) -} lam_ccs

--appendCCS :: Stack -> Stack -> Stack
--appendCCS ccs ["CAF"] = ccs
--appendCCS ccs (cc:ccs') = pushCC cc (appendCCS ccs ccs')
{-
pushCC cc ccs
  | Just trunc <- findCC cc ccs = trunc
  | otherwise                   = cc:ccs

findCC cc [] = Nothing
findCC cc (cc':ccs)
  | cc == cc'  = Just (cc':ccs)
  | otherwise  = findCC cc ccs
-}
-- -----------------------------------------------------------------------------
-- Misc.

modifyHeap f = modify (\(h,i) -> (f h, i))

lookupHeap :: Var -> E (Stack,Expr)
lookupHeap x = do
   (h,i) <- get
   case Map.lookup x h of
	Just z -> return z
	Nothing -> error ("unbound variable " ++ show x)

update x val = modifyHeap (\h -> Map.insert x val h)

eHat :: Expr -> E Expr
eHat e = do
  (h,i) <- get
  let (e',i') = runState (hat e) i
  put (h,i')
  return e'

