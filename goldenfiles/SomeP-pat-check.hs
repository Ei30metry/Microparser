-- gtype Some :: Type -> Type
-- gdata MkSome :: forall u e. Eq u => Show e => u -> e -> Some u
-- gpat bidir 2 SomeP :: forall u. Ord u => forall e. Eq u => Show e => u -> e -> Some u
-- tester :: forall u. Ord u => forall e. Eq u => Show e => u -> e -> Some u
-- tester = undefined
gpat bidir 2 SomeP :: forall u. Ord u => Eq u => Show e => u -> e -> Some u
