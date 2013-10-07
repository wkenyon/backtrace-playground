module Syntax where

import Control.Monad
import Control.Monad.State
import Text.PrettyPrint
-- -----------------------------------------------------------------------------

type Prog = [Bind]
type Prog1 = [Bind1]

type Bind = (Var,Expr)
type Bind1 = (Var, Expr1)

type Var = String
type Cons = (String,[Var])
type Alt = (Cons,Expr)
type Alt1 = (Cons,Expr1)

type Label = String


data Expr1
  = EVar1 Var
  | EInt1 Int
  | ELam1 Var Expr1
  | ECons1 Cons
  | ELet1 Bind1 Expr1
  | EPlus1 Expr1 Expr1
  | EApp1 Expr1 Expr1
  | EPush1 Label Expr1
  | ECase1 Expr1 [Alt1]
  | EBreak1 Expr1
  deriving Eq

data Expr
  = EVar Var
  | EInt Int
  | ELam Var Expr
  | ECons Cons
  | ELet Bind Expr
  | EPlus Expr Expr
  | EApp Expr Var
  | EPush Label Expr
  | ECase Expr [Alt]
  | EBreak Expr
  deriving Eq

type Fresh = State Int
instance Show Expr where
  show e = render (pprExpr e)

-- ----------------------------------------------------------------------------
--Launchbury 94's * transformation
starProg :: Prog1 -> Fresh Prog
starProg bs = mapM starBind bs

starBind :: Bind1 -> Fresh Bind
starBind (x,e) = liftM (\e1->(x,e1)) (star e)

star :: Expr1 -> Fresh Expr
star (EVar1 a) = return $ EVar a
star (EInt1 a) = return $ EInt a
star (ECons1 a) = return $ ECons a
star (ELam1 s e) = return (ELam s) `ap` star e
star (ELet1 (s,e1) e2) = return (\e1 -> ELet (s,e1)) `ap` star e1 `ap` star e2
star (EPlus1 e1 e2) = return EPlus `ap` star e1 `ap` star e2
star (EApp1 e1 (EVar1 x)) = return EApp `ap` star e1 `ap` return x
star (EApp1 e1 e2) = do e1star <- star e1
                        e2star <- star e2
                        i <- getFresh
                        let var = "s_" ++ show i
                        return (ELet (var,e2star) (EApp e1star var))
star (EPush1 l e) = return (EPush l) `ap` (star e)
star (ECase1 e alts) = do estar <- star e
                          altsstar <- forM alts $ \(c,e1) -> do
                            e1star <- star e1
                            return (c,e1star)
                          return $ ECase estar altsstar

star (EBreak1 e) = return EBreak `ap` star e

-- -----------------------------------------------------------------------------
--Launchbury 94's ^ transformation
getFresh :: Fresh Int
getFresh = modify succ >> get

hat :: Expr -> Fresh Expr
hat (EVar a) = return $ EVar a
hat (EInt a) = return $ EInt a
hat (ECons a) = return $ ECons a

hat (ELam s e) = do
  i <- getFresh
  e <- hat e
  let s' = (s ++ "_" ++ (show i))
  let e' = subst s s' e
  return $ ELam s' e'

hat (ELet (s,e1) e2) = do 
  i <- getFresh
  e1 <- hat e1
  e2 <- hat e2
  let s' = (s ++ "_" ++ (show i))
  let e2' = subst s s' e2
  return $ ELet (s',e1) e2'

hat (EPlus e1 e2) = do
  e1 <- hat e1
  e2 <- hat e2
  return $ EPlus e1 e2

hat (EApp e v) = do
  e <- hat e
  return $ EApp e v

hat (EPush l e) = do
  e <- hat e
  return $ EPush l e

hat (EBreak e) = do 
  e <- hat e
  return $ EBreak e

hat (ECase e alts) = do
  e <- hat e
  alts <- forM alts $ \((c,vars),e2) -> (do
    e2 <- hat e2
    fs <- mapM (\_->getFresh) vars
    let vars' = zipWith (++) vars $ map show fs
    let e2' = substList vars vars' e2
    return ((c,vars'),e2')
    )
  return $ ECase e alts

pprProg :: [Bind] -> Doc
pprProg binds = vcat (map (<> semi) (map pprBind binds))

pprBind :: Bind -> Doc
pprBind (s,e) = sep [ text s <+> char '=', nest 2 (pprExpr e) ]

pprCons :: Cons -> Doc
pprCons (s,vars) = text s <+> (sep $ map text vars )

pprAlts :: [Alt] -> Doc
pprAlts alts = char '{' <+> (sep $ map f alts) <+> char '}'
  where f (cs,expr) = pprCons cs <+> char '.' <+> pprExpr expr <+> char ';'

pprExpr :: Expr -> Doc
pprExpr (EVar s) = text s
pprExpr (EInt i) = int i
pprExpr (ELam s e) = char '\\' <> text s <> char '.' <+> pprExpr e
pprExpr (ELet b e2) = sep [ text "let", nest 2 (pprBind b), text "in",
			    nest 2 (pprExpr e2) ]
pprExpr (EPlus e1 e2) = pprExpr2 e1 <+> text "+" <+> pprExpr e2
pprExpr (EApp e s) = pprExpr2 e <+> text s
pprExpr (EPush x e) = sep [ text "scc" <+> text x, nest 2 (pprExpr e) ]
pprExpr (ECons cs) = pprCons cs
pprExpr (ECase e alts) = text "case" <+> pprExpr e <+> text "of" <+> pprAlts alts
pprExpr (EBreak e) = text "break" <+> pprExpr e
pprExpr2 e@ELet{}  = parens (pprExpr e)
pprExpr2 e@EPush{}  = parens (pprExpr e)
pprExpr2 e@EPlus{} = parens (pprExpr e)
pprExpr2 e = pprExpr e

pprProg1 :: [Bind1] -> Doc
pprProg1 binds = vcat (map (<> semi) (map pprBind1 binds))

pprBind1 :: Bind1-> Doc
pprBind1 (s,e) = sep [ text s <+> char '=', nest 2 (pprExpr1 e) ]

pprAlts1 :: [Alt1] -> Doc
pprAlts1 alts = char '{' <+> (sep $ map f alts) <+> char '}'
  where f (cs,expr) = pprCons  cs <+> char '.' <+> pprExpr1 expr <+> char ';'

pprExpr1 :: Expr1 -> Doc
pprExpr1 (EVar1 s) = text s
pprExpr1 (EInt1 i) = int i
pprExpr1 (ELam1 s e) = char '\\' <> text s <> char '.' <+> pprExpr1 e
pprExpr1 (ELet1 b e2) = sep [ text "let", nest 2 (pprBind1 b), text "in",
			    nest 2 (pprExpr1 e2) ]
pprExpr1 (EPlus1 e1 e2) = pprExpr21 e1 <+> text "+" <+> pprExpr1 e2
pprExpr1 (EApp1 e e1) = pprExpr21 e <+> pprExpr21 e1
pprExpr1 (EPush1 x e) = sep [ text "scc" <+> text x, nest 2 (pprExpr1 e) ]
pprExpr1 (ECons1 cs) = pprCons  cs
pprExpr1 (ECase1 e alts) = text "case" <+> pprExpr1 e <+> text "of" <+> pprAlts1 alts
pprExpr1 (EBreak1 e) = text "break" <+> pprExpr1 e
pprExpr21 e@ELet1{}  = parens (pprExpr1 e)
pprExpr21 e@EPush1{}  = parens (pprExpr1 e)
pprExpr21 e@EPlus1{} = parens (pprExpr1 e)
pprExpr21 e = pprExpr1 e

--pprExpr3 e@ELet{}  = parens (pprExpr e)
--pprExpr3 e@EApp{}  = parens (pprExpr e)
--pprExpr3 e@EPush{} = parens (pprExpr e)
--pprExpr3 e@EPlus{} = parens (pprExpr e)
--pprExpr3 e = pprExpr e

substList :: [Var] -> [Var] -> Expr -> Expr
substList xs ys e = foldr (\(x,y) e1 -> subst x y e1) e $ zip xs ys

subst :: Var -> Var -> Expr -> Expr
subst x y (EVar z)	= EVar (substVar x y z)
subst x y (EInt i)	= EInt i
subst x y (ELam z e)	= ELam z (subst x y e)  -- ToDo: name capture?
subst x y (ELet (z,e1) e2) = ELet (z,subst x y e1) (subst x y e2)
subst x y (EPlus e1 e2)  = EPlus (subst x y e1) (subst x y e2)
subst x y (EApp f z)	= EApp (subst x y f) (substVar x y z)
subst x y (EPush cc e)	= EPush cc (subst x y e)
subst x y (EBreak e)    = EBreak (subst x y e)
subst x y (ECons cs) = ECons $ substCons x y cs
subst x y (ECase e alts) = ECase (subst x y e) $ map f alts
  where f (cs,expr) = (substCons x y cs, subst x y expr)

substCons x y (s,vars) = (s,map (\z->substVar x y z) vars)

substVar x y z
  | z == x 		= y
  | otherwise		= z
