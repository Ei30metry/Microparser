data Some a where
  MkSome :: forall u e. (Eq u, Show e) => u -> e -> Some u

pattern SomeP :: forall u. Ord u =>
                 forall e. (Eq u, Show e) => u ->
                 e -> Some u

pattern SomeP uni ex = MkSome uni ex

useSome :: forall u. Ord u => Some u -> String
useSome (SomeP uni ex) = show ex
