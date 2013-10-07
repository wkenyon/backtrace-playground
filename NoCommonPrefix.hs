module NoCommonPrefix where
import Syntax
import Text.PrettyPrint

type Stack = [Label]

topStack = ["TOPSTACK"]

cafLabel  = "CAF"
cafStack = [cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'

showCCS = render . pprStack

pushCC :: Label -> Stack -> Stack
pushCC = (:)

funCall :: Stack -> Stack -> Stack
funCall = swap (++)
swap f x y = f y x

