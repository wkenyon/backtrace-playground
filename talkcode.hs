eval :: Stack -> Expr -> E (Stack,Expr)

eval stk (EInt i)    = return (stk, EInt i)
eval stk (ELam x e)  = return (stk, ELam x e)

eval stk (EPush l e) = eval (push l stk) e

eval stk (ELet (x,e1) e2) = do
   insertHeap x (stk,e1)
   eval stk e2

eval stk (EPlus e1 e2) = do
   (_,EInt x) <- eval stk e1
   (_,EInt y) <- eval stk e2
   tick stk
   return (stk, EInt (x+y))

eval stk (EApp f x) = do
   (lam_stk, ELam y e) <- eval stk f
   eval lam_stk (subst y x e)

eval stk (EVar x)  = do
   r <- lookupHeap x
   case r of
     (stk', EInt i) ->
        return (stk', EInt i)
     
     (stk', ELam y e) ->
        enter stk stk' x (ELam y e)
     
     (stk',e) -> do 
        deleteHeap x
        (stkv, v) <- eval stk' e
        insertHeap x (stkv,v)
        return (enter stk stkv v)

enter stk stkv (EInt i)   = (stkv, EInt i)
enter stk stkv (ELam y e) = (funCall stk stkv, ELam y e)
