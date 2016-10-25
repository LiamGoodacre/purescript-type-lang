module Main where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- | Natural numbers
-- | Used for variables counting out lambda binders

class Nat d

data Zero
instance natZero
  :: Nat Zero

data Succ n
instance natSucc
  :: Nat n => Nat (Succ n)

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four


-- | Expressions
-- | Terms in our language

class Expr d

data Var n
instance exprVar
  :: Nat n
  => Expr (Var n)

data Lam b
instance exprLam
  :: Expr b
  => Expr (Lam b)

data App l r
instance exprApp
  :: (Expr l, Expr r)
  => Expr (App l r)

data Str (b :: Symbol)
instance exprStr
  :: Expr (Str b)

data Cat l r
instance exprCat
  :: (Expr l, Expr r)
  => Expr (Cat l r)

data Type ty
instance exprType
  :: Expr (Type ty)

data Type1 (ty1 :: * -> *)
instance exprType1
  :: Expr (Type1 ty1)

data AppType1 ty1 ty
instance exprAppType1
  :: (Expr ty1, Expr ty)
  => Expr (AppType1 ty1 ty)


-- | Values
-- | Results of evaluating an expression in a context

class Value d

data FunVal context body
instance valueFunVal
  :: (Context context, Expr body)
  => Value (FunVal context body)

data StrVal (string :: Symbol)
instance valueStrVal
  :: Value (StrVal string)

data TypeVal ty
instance valueTypeVal
  :: Value (TypeVal ty)

data Type1Val (ty1 :: * -> *)
instance valueType1Val
  :: Value (Type1Val ty1)


-- | Context
-- | A list of values bound by lambdas

class Context d

data Empty
instance contextEmpty
  :: Context Empty

data Extend value tail
instance contextExtend
  :: (Value value, Context tail)
  => Context (Extend value tail)


-- Evaluator

class Eval context expr output | context expr -> output

instance evalVarZero
  :: (Value value,
      Context tail)
  => Eval (Extend value tail) (Var Zero) value

instance evalVarSucc
  :: (Nat num,
      Context tail,
      Value ignored,
      Value value,
      Eval tail (Var num) value)
  => Eval (Extend ignored tail) (Var (Succ num)) value

instance evalLam
  :: (Context context,
      Expr body)
  => Eval context (Lam body) (FunVal context body)

instance evalApp
  :: (Context context,
      Context funContext,
      Expr body,
      Eval context f (FunVal funContext body),
      Value arg,
      Eval context x arg,
      Value output,
      Eval (Extend arg funContext) body output)
  => Eval context (App f x) output

instance evalType
  :: Context context
  => Eval context (Type ty) (TypeVal ty)

instance evalType1
  :: Context context
  => Eval context (Type1 ty1) (Type1Val ty1)

instance evalAppType1
  :: (Context context,
      Eval context f (Type1Val ty1),
      Eval context x (TypeVal ty))
  => Eval context (AppType1 f x) (TypeVal (ty1 ty))

instance evalStr
  :: Context context
  => Eval context (Str b) (StrVal b)

instance evalCat
  :: (Eval context l (StrVal left),
      Eval context r (StrVal right))
  => Eval context (Cat l r) (StrVal (CatSym left right))

-- replacement for `TypeConcat` until that gets an `IsSymbol` instance
foreign import data CatSym :: Symbol -> Symbol -> Symbol
instance isSymbolCatSym :: (IsSymbol l, IsSymbol r) => IsSymbol (CatSym l r) where
  reflectSymbol SProxy =
    reflectSymbol (SProxy :: SProxy l) <> reflectSymbol (SProxy :: SProxy r)


class RunType expr output | expr -> output
instance runTypeEval
  :: Eval Empty expr (TypeVal output) => RunType expr output

runType :: forall expr output r.
  (Expr expr,
   RunType expr output) =>
  Proxy expr -> (output -> r) -> output -> r
runType Proxy f = f


class RunSymbol expr (output :: Symbol) | expr -> output
instance runSymbolEval
  :: Eval Empty expr (StrVal output) => RunSymbol expr output

runSymbol :: forall expr output.
  (Expr expr,
   RunSymbol expr output) =>
  Proxy expr -> SProxy output
runSymbol Proxy = SProxy

runString :: forall expr output.
  (Expr expr,
   IsSymbol output,
   RunSymbol expr output) =>
  Proxy expr -> String
runString = reflectSymbol <<< runSymbol


-- | Examples

type Id = Lam (Var Zero)
type Const = Lam (Lam (Var One))
type Concat = Lam (Lam (Cat (Var Zero) (Var One)))

type True = Lam (Lam (Var One))
type False = Lam (Lam (Var Zero))
type If b t e = App (App b t) e

type Nil = Lam (Lam (Var Zero))
type Cons h t = Lam (Lam (App (App (Var One) h) (App (App t (Var One)) (Var Zero))))
type Fold c n xs = App (App xs c) n
type JoinStrings xs = Fold (Lam (Lam (Cat (Var One) (Var Zero)))) (Str "") xs

type Hello = App Id (Str "Hello")
type World = Str "World"
type HelloWorld = App (App Concat Hello) World
type X = Cat (If False (Str "Yeah") (Str "Nooo")) (Str "!!!")
type Y = JoinStrings (Cons (Str "A") (Cons X (Cons HelloWorld Nil)))

-- inspect the computed type of `p`
p :: SProxy _
p = runSymbol (Proxy :: Proxy Y)


-- AppNTypes [f, g, ..., h] x -> f (g (... (h x)))
type AppNTypes xs u = Fold (Lam (Lam (AppType1 (Var One) (Var Zero)))) u xs
-- CollM = [Maybe, Maybe]
type CollM = Cons (Type1 Maybe) (Cons (Type1 Maybe) Nil)
-- CollP = [Proxy, Proxy, Proxy]
type CollP = Cons (Type1 Proxy) (Cons (Type1 Proxy) (Cons (Type1 Proxy) Nil))
type MM u = AppNTypes CollM u
type PPP u = AppNTypes CollP u

-- program building the type `Maybe (Maybe (Proxy (Proxy (Proxy Unit))))`
maybe2Proxy3Type :: Proxy (MM (PPP (Type Unit)))
maybe2Proxy3Type = Proxy

-- evaluate maybe2Proxy3Type and use the resulting type
computedType :: _
computedType = runType maybe2Proxy3Type (case _ of
  Nothing -> 0
  Just Nothing -> 1
  Just (Just Proxy) -> 2)


helloWorld :: String
helloWorld = reifySymbol "Hello" (\o -> reifySymbol "World" \t -> one o t) where
  one :: forall l r. (IsSymbol l, IsSymbol r) => SProxy l -> SProxy r -> String
  one _ _ = runString (Proxy :: Proxy (Cat (Str l) (Str r)))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log helloWorld
