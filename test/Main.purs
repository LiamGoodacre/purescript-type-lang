module Test.Main where

import Prelude
import Type.Lang.Types (V0, V1, Lam, App, Typ, Typ1, If, Bool, False, Sym, Cat)
import Type.Lang.Eval (runType, runString)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, SProxy, reifySymbol)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- | Examples
type Nil = Lam (Lam V0)
type Cons h t = Lam (Lam (App (App V1 h) (App (App t V1) V0)))
type Fold c n xs = App (App xs c) n
type JoinStrings xs = Fold (Lam (Lam (Cat V1 V0))) (Sym "") xs

type Hello = Sym "Hello"
type World = Sym "World"
type Concat = Lam (Lam (Cat V0 V1))
type HelloWorld = App (App Concat Hello) World
type X = Cat (If (Bool False) (Sym "Yeah") (Sym "Nooo")) (Sym "!!!")
type Y = JoinStrings (Cons (Sym "A") (Cons X (Cons HelloWorld Nil)))

-- AppNTypes [f, g, ..., h] x -> f (g (... (h x)))
type AppNTypes xs u = Fold (Lam (Lam (App V1 V0))) u xs
-- CollM = [Maybe, Maybe]
type CollM = Cons (Typ1 Maybe) (Cons (Typ1 Maybe) Nil)
-- CollP = [Proxy, Proxy, Proxy]
type CollP = Cons (Typ1 Proxy) (Cons (Typ1 Proxy) (Cons (Typ1 Proxy) Nil))
type MM u = AppNTypes CollM u
type PPP u = AppNTypes CollP u

-- program building the type `Maybe (Maybe (Proxy (Proxy (Proxy Unit))))`
maybe2Proxy3Type :: Proxy (MM (PPP (Typ Unit)))
maybe2Proxy3Type = Proxy

-- evaluate maybe2Proxy3Type and use the resulting type
computedType :: _
computedType = runType maybe2Proxy3Type (case _ of
  Nothing -> 0
  Just Nothing -> 1
  Just (Just x) -> 2)

helloWorld :: String
helloWorld = reifySymbol "Hello" (\o -> reifySymbol "World" \t -> one o t) where
  one :: forall l r. (IsSymbol l, IsSymbol r) => SProxy l -> SProxy r -> String
  one _ _ = runString (Proxy :: Proxy (Cat (Sym l) (Sym r)))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log helloWorld
