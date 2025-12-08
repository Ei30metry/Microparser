{-# LANGUAGE DataKinds #-}
-- | TODO: Delete me

module Language.MicroHs.Test where

import qualified Language.MicroHs.Parse        as P
import qualified Language.MicroHs.Lex          as L
import qualified Language.MicroHs.Ident        as I
import qualified Language.MicroHs.Expr         as E
import qualified Text.PrettyPrint.HughesPJLite as HP

tyAbs1 = "tyAbs1 @a  x = e"
tyAbs2 = "tyAbs2 @ty @te x y = x + y"
tyAbs3 = "tyAbsId @b x = x :: b"
tyAbs4 = "tyAbsMaybeInteger @(Maybe Integer) x = True"

tyApp1 = "tyApp1 = id @Integer"
tyApp2 = "tyApp2 = id @(FooMap (Maybe String))"

asPat1 = "asPat1 x@(Just y) = y"
asPat2 = "asPat2 x@y = (x, y)"

eqn1   = "id x = x"
eqn2   = "cons x y = x"

dataGADT1 = "data Blah a where MkBlah1 :: Int -> Blah Int"
dataNormal1 = "data Blah a = MkBlah1 Int | MkBlah2 String | MkBlah3 a"

gammaBindings = "gamma_bindings where datag Just :: forall a. a -> Maybe a"

kindSig1 = "type Maybe :: Type -> Type"
kindSig2 = "type Foo :: forall a. a -> Type"

defaultLexer = L.lex (I.SLoc "Test" 1 1)

parseTyAbs1 = parseHsString tyAbs1
parseTyAbs2 = parseHsString tyAbs2
parseTyAbs3 = parseHsString tyAbs3
parseTyAbs4 = parseHsString tyAbs4

parseEqn1  = parseHsString eqn1
parseEqn2  = parseHsString eqn2

parseDataGADT1 = parseDataDecls dataGADT1
parseNormal1   = parseDataDecls dataNormal1

parseGammaBindings = parseHsString

lambdaCases1 = "f = \\cases Nothing -> Just (x + y)"
lambdaCase1  = "f = \\case Just x -> Just (x + 1)"

lexTests verbose
  = HP.render
  . HP.vcat
  $ map pprTest [("lambdaCases1", lambdaCases1)]
  where
    tests = [("tyAbs1", tyAbs1), ("tyAbs2", tyAbs2), ("tyAbs3", tyAbs3), ("tyAbs4", tyAbs4)
                ,("tyApp1", tyApp1), ("tyApp2", tyApp2)
                ,("asPat1", asPat1), ("asPat2", asPat2)
                ,("gammaBindings", gammaBindings)]
    pprTest (testName, test)
      = HP.text testName HP.<+>
        HP.nest 1 (HP.hcat  .
                   HP.punctuate (HP.text ",") .
                   map (HP.text . if verbose then show else L.showToken) $
                   defaultLexer test)

type ESign         = ([I.Ident], E.EType)
type EFunBn        = (I.Ident, [E.Eqn])
type EData         = (E.LHS, [E.Constr])
type EKindSig      = (I.Ident, E.EType)
type EGammaTyCon   = (I.Ident, E.EType)
type EGammaDataCon = (I.Ident, E.EType)

parseDataDecls = fmap (\(_, _, _, ds, _) -> ds) . parseHsString
parseFunBinds  = fmap (\(_, _, fs, _, _) -> fs) . parseHsString
parseTySigs    = fmap (\(_, ss, _, _, _) -> ss) . parseHsString
parseKindSigs  = fmap (\(ks, _, _, _, _) -> ks) . parseHsString
parseLangExts  = fmap (\(_, _, _, _, es) -> es) . parseHsString

langExtFile = readFile "/Users/artin/Programming/projects/MicroHs-prelude/lib/Microparser/goldenfiles/LangExt1.txt"

foo = parseLangExts <$> langExtFile

parseHsString :: String -> Either String ([EKindSig], [ESign], [EFunBn], [EData], [E.ELangExt])
parseHsString str = case P.parse P.pTop "" str of
  Left x                          -> Left x
  Right (E.EModule _ _ defs exts) -> Right $ (\(a, b, c, d) -> (a, b, c, d, exts)) res
    where
      res = go ([], [], [], []) defs
      go (ks, ss, fs, cls) (E.Fcn idt eqs : ds)
        = go (ks, ss, (idt, eqs) : fs, cls) ds
      go (ks, ss, fs, cls) (E.Sign idts ety : ds)
        = go (ks, (idts, ety) : ss, fs, cls) ds
      go (ks, ss, fs, cls) (E.Data lhs rhs _ : ds)
        = go (ks, ss, fs, (lhs, rhs) : cls) ds
      go (ks, ss, fs, cls) (E.KindSign ident ty : ds)
        = go ((ident, ty) : ks, ss, fs, cls)ds
      go (ks, ss, fs, cls) (_ : ds)
        = go (ks, ss, fs, cls) ds
      go acc []
        = acc
