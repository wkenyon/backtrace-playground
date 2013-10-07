module CostCentreStack where

type Label = String

class CostCentreStack s where
  cafStack :: s
  topStack :: s
  pprStack :: s->Doc
  pushCC   :: Label -> CostCentreStack -> CostCentreStack
  funCall  :: CostCentreStack -> CostCentreStack -> CostCentreStack