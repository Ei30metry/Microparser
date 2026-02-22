gtype GADTVDQTwo :: Type -> Type -> Type
gdata MkGADTVDQTwo :: forall a. forall b -> Tuple2 a b -> GADTVDQTwo a b

unwrapGADTVDQTwo :: forall a b. GADTVDQTwo a b -> Tuple2 b a
unwrapGADTVDQTwo (MkGADTVDQTwo @a b c) = MkTuple2 (snd c :: b) (fst c :: a)
