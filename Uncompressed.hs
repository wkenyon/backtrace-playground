module Novel where

import Syntax
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as Multi
import Text.PrettyPrint
import Data.Maybe

type Stack = [Label]

topStack = ["TOPSTACK"]

cafLabel  = "CAF"
cafStack = [cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'

showCCS = render . pprStack

-- -----------------------------------------------------------------------------
-- Stack operations
pushCC :: Label -> Stack -> Stack
pushCC = (:)

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