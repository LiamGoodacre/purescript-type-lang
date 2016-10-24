module Main where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- | Natural numbers
-- | Used for variables counting out lambda binders

class Nat d

data Zero = NatZero
instance natZero
  :: Nat Zero

data Succ n = NatSucc
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

data Var n = Var
instance exprVar
  :: Nat n
  => Expr (Var n)

data Lam b = Lam
instance exprLam
  :: Expr b
  => Expr (Lam b)

data App l r = App
instance exprApp
  :: (Expr l, Expr r)
  => Expr (App l r)

data Str (b :: Symbol) = Str
instance exprStr
  :: Expr (Str b)

data Cat l r
instance exprCat
  :: (Expr l, Expr r)
  => Expr (Cat l r)


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


eval :: forall expr output.
  (Expr expr,
   Eval Empty expr (StrVal output)) =>
  Proxy expr -> SProxy output
eval Proxy = SProxy

evalString :: forall expr output.
  (Expr expr,
   IsSymbol output,
   Eval Empty expr (StrVal output)) =>
  Proxy expr -> String
evalString = reflectSymbol <<< eval


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
type Map f xs = Fold (Lam (Lam (Cons (App f (Var One)) (Var Zero)))) Nil xs
type JoinStrings xs = Fold (Lam (Lam (Cat (Var One) (Var Zero)))) (Str "") xs

type Hello = App Id (Str "Hello")
type World = Str "World"
type HelloWorld = App (App Concat Hello) World
type X = Cat (If False (Str "Yeah") (Str "Nooo")) (Str "!!!")
type Y = JoinStrings (Cons (Str "A") (Cons X (Cons HelloWorld Nil)))

-- inspect the computed type of `p`
p :: SProxy _
p = eval (Proxy :: Proxy Y)

helloWorld :: String
helloWorld = reifySymbol "Hello" (\o -> reifySymbol "World" \t -> one o t) where
  one :: forall l r. (IsSymbol l, IsSymbol r) => SProxy l -> SProxy r -> String
  one _ _ = evalString (Proxy :: Proxy (Cat (Str l) (Str r)))


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log helloWorld
