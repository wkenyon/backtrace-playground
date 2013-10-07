module Novel where

import Syntax
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Multi
import Text.PrettyPrint
import Data.Maybe

type Stack = [Stack']
data Stack' = Elision
            | Basic Label
  deriving (Show, Ord, Eq)

topStack = [Basic "TOPSTACK"]

cafLabel  = "CAF"
cafStack = [Basic cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map pprStack' (reverse ccs))) <> char '>'

pprStack' (Basic cc) = text cc
pprStack' Elision = text "{...}"

showCCS = render . pprStack

-- -----------------------------------------------------------------------------
-- Stack operations
pushCC :: Label -> Stack -> Stack
pushCC = (.::) . Basic


(.::) :: Stack' -> Stack -> Stack
Elision  .:: (Elision : xs) = Elision : xs
Elision  .:: xs = Elision:xs
b@(Basic cc)  .:: xs = (b:) . (foldl g []) . reverse . (map f) $ xs
  where 
    f Elision = Elision
    f (Basic cc') | cc' == cc = Elision 
                  | otherwise = Basic cc'
    g (Elision:ys) Elision = Elision : ys
    g ys y = y : ys


funCall :: Stack -> Stack -> Stack
funCall ccs lam_ccs = (reverse lam_ccs') +++ ccs
  where
    (pre, ccs', lam_ccs') = commonPrefix (reverse ccs) (reverse lam_ccs)

(+++) :: Stack -> Stack -> Stack    
[] +++ ys = ys
(x:xs) +++ ys = x .:: (xs+++ys) 

commonPrefix :: Eq a => [a] -> [a] -> ([a],[a],[a])
commonPrefix xs ys = f [] xs ys
  where 
    f pre [] ys = (pre,[],ys)
    f pre xs [] = (pre,xs,[])
    f pre (x:xs) (y:ys) | x==y = f (x:pre) xs ys
                            | otherwise = (pre,x:xs,y:ys) 