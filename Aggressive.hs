module Aggressive where
import Syntax
import Text.PrettyPrint

type Stack = [Label]

topStack = ["TOPSTACK"]

cafLabel  = "CAF"
cafStack = [cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'

showCCS = render . pprStack

funCall ccs lam_ccs
  | not (null lam_ccs) && last lam_ccs == "CAF"  
  = {- trace ("funCall, ccs = " ++ show ccs) $ -} ccs `appendCCS` lam_ccs
  | otherwise
  = {- trace ("funCall, lam_ccs = " ++ show lam_ccs) -} lam_ccs

appendCCS :: Stack -> Stack -> Stack
appendCCS ccs ["CAF"] = ccs
appendCCS ccs (cc:ccs') = pushCC cc (appendCCS ccs ccs')

pushCC cc ccs
  | Just trunc <- findCC cc ccs = trunc
  | otherwise                   = cc:ccs

findCC cc [] = Nothing
findCC cc (cc':ccs)
  | cc == cc'  = Just (cc':ccs)
  | otherwise  = findCC cc ccs