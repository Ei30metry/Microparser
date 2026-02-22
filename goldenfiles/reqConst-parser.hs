-- https://github.com/augustss/MicroHs/issues/209
reqConst :: forall a -> forall b -> a -> b -> a
reqConst x y = const @x @y
