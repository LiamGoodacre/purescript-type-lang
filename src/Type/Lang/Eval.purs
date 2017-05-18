module Type.Lang.Eval where

import Type.Lang.Types
import Prelude ((<<<))
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- Evaluator

class Eval context expr output | context expr -> output

instance evalVarHere
  :: (IsValue value,
      IsContext tail)
  => Eval (Extend value tail) (Var Here) value

instance evalVarThere
  :: (IsVarRef ref,
      IsContext tail,
      IsValue ignored,
      IsValue value,
      Eval tail (Var ref) value)
  => Eval (Extend ignored tail) (Var (There ref)) value

instance evalLam
  :: (IsContext context,
      IsExpr body)
  => Eval context (Lam body) (FunVal context body)

class EvalApplication f x o | f x -> o
instance evalApplicationLam
  :: (IsContext context,
      IsExpr body,
      IsValue arg,
      IsValue output,
      Eval (Extend arg context) body output)
  => EvalApplication (FunVal context body) arg output
instance evalApplicationTyp
  :: EvalApplication (Typ1Val typ1) (TypVal typ) (TypVal (typ1 typ))

instance evalApp
  :: (IsContext context,
      IsValue funVal,
      Eval context fun funVal,
      IsValue argVal,
      Eval context arg argVal,
      IsValue output,
      EvalApplication funVal argVal output)
  => Eval context (App fun arg) output

instance evalTyp
  :: IsContext context
  => Eval context (Typ typ) (TypVal typ)

instance evalTyp1
  :: IsContext context
  => Eval context (Typ1 typ1) (Typ1Val typ1)

instance evalSym
  :: IsContext context
  => Eval context (Sym sym) (SymVal sym)

instance evalCat
  :: (IsContext context,
      Eval context l (SymVal left),
      Eval context r (SymVal right))
  => Eval context (Cat l r) (SymVal (TypeConcat left right))

instance evalBool
  :: (IsContext context,
      IsBool bool)
  => Eval context (Bool bool) (BoolVal bool)

class EvalIfCondition b t e o | b -> t e o
instance ifConditionTrue :: EvalIfCondition True t e t
instance ifConditionFalse :: EvalIfCondition False t e e

instance evalIf
  :: (IsContext context,
      IsBool bool,
      Eval context condition (BoolVal bool),
      EvalIfCondition bool ifTrue ifFalse branch,
      Eval context branch output)
  => Eval context (If condition ifTrue ifFalse) output


-- | Evaluate to a type

class RunType expr output | expr -> output
instance runTypeEval
  :: Eval Empty expr (TypVal output)
  => RunType expr output

runType :: forall expr output r.
  IsExpr expr =>
  RunType expr output =>
  Proxy expr -> (output -> r) -> output -> r
runType Proxy f = f

runTypeOn :: forall expr output r.
  IsExpr expr =>
  RunType expr output =>
  expr -> (output -> r) -> output -> r
runTypeOn _ f = f


-- | Evaluate to a symbol

class RunSymbol expr (output :: Symbol) | expr -> output
instance runSymbolEval
  :: Eval Empty expr (SymVal output)
  => RunSymbol expr output

runSymbol :: forall expr output.
  IsExpr expr =>
  RunSymbol expr output =>
  Proxy expr -> SProxy output
runSymbol Proxy = SProxy

runString :: forall expr output.
  IsExpr expr =>
  IsSymbol output =>
  RunSymbol expr output =>
  Proxy expr -> String
runString = reflectSymbol <<< runSymbol

