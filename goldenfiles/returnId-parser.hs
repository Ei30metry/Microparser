-- https://github.com/augustss/MicroHs/issues/209
returnId :: forall a. a -> forall b. b -> b
returnId _ = id
