{-#LANGUAGE Rank2Types, ExistentialQuantification,TypeSynonymInstances,FlexibleInstances,InstanceSigs #-}

module EvalTypes where

import Syntax
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Multi
import Text.PrettyPrint
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)


type Heap = Map Var (All,Expr)

class CostCentreStack s where
  cafStack :: s
  topStack :: s
  pprStack :: s->Doc
  pushCC   :: Label -> s -> s
  funCall  :: s -> s -> s

showCCS :: forall a . CostCentreStack a => a -> String
showCCS = render . pprStack

commonPrefix :: Eq a => [a] -> [a] -> ([a],[a],[a])
commonPrefix xs ys = f [] xs ys
  where 
    f pre [] ys = (pre,[],ys)
    f pre xs [] = (pre,xs,[])
    f pre (x:xs) (y:ys) | x==y = f (x:pre) xs ys
                        | otherwise = (pre,x:xs,y:ys) 




type All = (Novel, Elision, Uncompressed)

instance CostCentreStack All where
    topStack :: All
    topStack = (topStack,topStack,topStack)

    cafStack :: All
    cafStack = (cafStack,cafStack,cafStack)

    pprStack :: All -> Doc
    pprStack (a,b,c) = vcat [pprStack a, pprStack b, pprStack c]

    pushCC :: Label -> All -> All
    pushCC label (a,b,c) = (pushCC label a, pushCC label b, pushCC label c)

    funCall :: All -> All -> All
    funCall (a,b,c) (d,e,f) = (funCall a d, funCall b e, funCall c f)



-------------------
-- --Start NOVEL --
-------------------


type Novel = [Novel']
data Novel' = Elision (MultiSet Label)
            | Basic Label
  deriving (Show, Ord, Eq)

instance CostCentreStack Novel where
  topStack = [Basic "TOPSTACK"]
  cafStack = []

  pprStack :: Novel -> Doc
  pprStack ccs = text "Novel:<" <> hcat (punctuate comma (map pprStack' (reverse ccs))) <> char '>'
    where
      pprStack' (Basic cc) = text cc
      pprStack' (Elision ms) = char '{' <> hcat (punctuate comma (map f (Multi.toOccurList ms))) <> char '}'
      f (cc,i) = text cc <+> int i

     

  pushCC :: Label -> Novel -> Novel
  pushCC = (.::) . Basic
  

  funCall :: Novel -> Novel -> Novel
  funCall ccs lam_ccs = (reverse lam_ccs') +++ ccs
    where
      (pre, ccs', lam_ccs') = commonPrefix (reverse ccs) (reverse lam_ccs)


(+++) :: Novel -> Novel -> Novel    
[] +++ ys = ys
(x:xs) +++ ys = x .:: (xs+++ys) 

(.::) :: Novel' -> Novel -> Novel
(Elision e1)  .:: (Elision e2 : xs) = (Elision $ e1 `Multi.union` e2) : xs
e@(Elision _) .:: xs = e:xs
b@(Basic cc)  .:: xs = (b:) . (foldl g []) . reverse . (map f) $ xs
  where 
    f (Elision e) = Elision e
    f (Basic cc') | cc' == cc = Elision $ Multi.singleton cc
                  | otherwise = Basic cc'
    g ((Elision e1):ys) (Elision e2) = (Elision $ e1 `Multi.union` e2) : ys
    g ys y = y : ys




---------------------
-- --Start elision --
---------------------




type Elision = [Maybe Label]

instance CostCentreStack Elision where
  topStack = [Just "TOPSTACK"]
  cafStack = []

  pprStack :: Elision -> Doc
  pprStack ccs = text "Elision: <" <> hcat (punctuate comma (map pprElision' (reverse ccs))) <> char '>'
    where
      pprElision' (Just cc) = text cc
      pprElision' Nothing = text "{...}"

  pushCC :: Label -> Elision -> Elision
  pushCC = (..::) . Just

  funCall :: Elision -> Elision -> Elision
  funCall ccs lam_ccs = (reverse lam_ccs') .+++ ccs
    where
      (pre, ccs', lam_ccs') = commonPrefix (reverse ccs) (reverse lam_ccs)

(.+++) :: Elision -> Elision -> Elision    
[] .+++ ys = ys
(x:xs) .+++ ys = x ..:: (xs.+++ys) 

(..::) :: Maybe Label -> Elision -> Elision
Nothing  ..:: (Nothing : xs) = Nothing : xs
Nothing  ..:: xs = Nothing:xs
b@(Just cc)  ..:: xs = (b:) . (foldl g []) . reverse . (map f) $ xs
  where 
    f Nothing = Nothing
    f (Just cc') | cc' == cc = Nothing 
                  | otherwise = Just cc'
    g (Nothing:ys) Nothing = Nothing : ys
    g ys y = y : ys

--------------------------
-- --start Uncompressed --
--------------------------

type Uncompressed = [Label]

instance CostCentreStack Uncompressed where

  topStack = ["TOPSTACK"]
  cafStack = []

  pprStack :: Uncompressed -> Doc
  pprStack ccs = text "Uncompressed: <" <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'


  pushCC :: Label -> Uncompressed -> Uncompressed
  pushCC = (:)

  funCall :: Uncompressed -> Uncompressed -> Uncompressed
  funCall ccs lam_ccs = (reverse lam_ccs') ++ ccs
    where
      (pre, ccs', lam_ccs') = commonPrefix (reverse ccs) (reverse lam_ccs)

----------------------------------------------------------------------------------------------------------------
-- ----------------------------                                                                               --
-- -- --start NoCommonPrefix --                                                                               --
-- ----------------------------                                                                               --
--                                                                                                            --
-- type NoCommonPrefix = [Label]                                                                              --
--                                                                                                            --
-- instance CostCentreStack NoCommonPrefix                                                                    --
--   where                                                                                                    --
--                                                                                                            --
--     topStack = ["TOPSTACK"]                                                                                --
--                                                                                                            --
--     cafStack = ["CAF"]                                                                                     --
--                                                                                                            --
--     pprStack :: NoCommonPrefix -> Doc                                                                      --
--     pprStack ccs = text "NoCommonPrefix: <"<> hcat (punctuate comma (map text (reverse ccs))) <> char '>'  --
--                                                                                                            --
--     pushCC :: Label -> NoCommonPrefix -> NoCommonPrefix                                                    --
--     pushCC = (:)                                                                                           --
--                                                                                                            --
--     funCall :: NoCommonPrefix -> NoCommonPrefix -> NoCommonPrefix                                          --
--     funCall = swap (++)                                                                                    --
--       where                                                                                                --
--         swap f x y = f y x                                                                                 --
--                                                                                                            --
-- ------------------------------------------                                                                 --
-- --       start Aggressive (Simon Marlow) --                                                                --
-- ------------------------------------------                                                                 --
--                                                                                                            --
-- type Aggressive = [Label]                                                                                  --
--                                                                                                            --
-- instance CostCentreStack Aggressive                                                                        --
--   where                                                                                                    --
--     topStack = []                                                                                          --
--     cafStack = ["CAF"]                                                                                     --
--     funCall ccs lam_ccs                                                                                    --
--       | not (null lam_ccs) && last lam_ccs == "CAF"                                                        --
--       = {- trace ("funCall, ccs = " ++ show ccs) $ -} ccs `appendCCS` lam_ccs                              --
--       | otherwise                                                                                          --
--       = {- trace ("funCall, lam_ccs = " ++ show lam_ccs) -} lam_ccs                                        --
--                                                                                                            --
--                                                                                                            --
--                                                                                                            --
--     pushCC cc ccs                                                                                          --
--       | Just trunc <- findCC cc ccs = trunc                                                                --
--       | otherwise                   = cc:ccs                                                               --
--                                                                                                            --
--     pprStack :: Aggressive -> Doc                                                                          --
--     pprStack ccs = text "Aggressive: <" <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'     --
--                                                                                                            --
-- findCC cc [] = Nothing                                                                                     --
-- findCC cc (cc':ccs)                                                                                        --
--   | cc == cc'  = Just (cc':ccs)                                                                            --
--   | otherwise  = findCC cc ccs                                                                             --
--                                                                                                            --
-- appendCCS :: Aggressive -> Aggressive -> Aggressive                                                        --
-- appendCCS ccs ["CAF"] = ccs                                                                                --
-- appendCCS ccs (cc:ccs') = pushCC cc (appendCCS ccs ccs')                                                   --
----------------------------------------------------------------------------------------------------------------


