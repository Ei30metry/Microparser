-- https://github.com/augustss/MicroHs/issues/209
tyAbsRejectDec13 :: forall b. b -> forall c. c -> b ~ c => Unit
tyAbsRejectDec13 (x :: a) @a (y :: a) = MkUnit
