module Test.Main where

import Type.Lang.Types
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Prelude (Unit, (+), discard)
import Type.Lang.Eval (runType, runTypeOn, runSymbol, runString, runRecordType)
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (SProxy)

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
computedType :: Maybe (Maybe (Proxy (Proxy (Proxy Unit)))) -> Int
computedType = runType maybe2Proxy3Type (case _ of
  Nothing -> 0
  Just Nothing -> 1
  Just (Just x) -> 2)

helloWorld :: String
helloWorld = runString (Proxy :: Proxy (Cat (Sym "Hello") (Sym "World")))

exampleRunType :: Int -> Int -> Int
exampleRunType = runType (Proxy :: Proxy (App (Lam V0) (Typ Int))) (+)

exampleRunTypeOn :: Int -> Int -> Int
exampleRunTypeOn = runTypeOn (App (Lam v0) (Typ :: Typ Int)) (+)


type RecordEG = ConsRec (Sym "foo") (Sym "A") (ConsRec (Sym "bar") (Sym "B") EmptyRec)
type IndexRecordEG = IndexRec (Sym "bar") RecordEG

exampleRunRecordType :: Proxy { foo :: SymVal "A", bar :: SymVal "B" }
exampleRunRecordType = runRecordType (Proxy :: Proxy RecordEG)

exampleRunRecordIndex :: SProxy "B"
exampleRunRecordIndex = runSymbol (Proxy :: Proxy IndexRecordEG)

exampleRunRecordIndexString :: String
exampleRunRecordIndexString = runString (Proxy :: Proxy IndexRecordEG)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log helloWorld
  log exampleRunRecordIndexString
