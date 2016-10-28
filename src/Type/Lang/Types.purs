module Type.Lang.Types where

-- | Variable references, currently equivalent to natural numbers.
class IsVarRef ctor

data Here
instance isVarRefHere
  :: IsVarRef Here

data There ref
instance isVarRefThere
  :: IsVarRef ref
  => IsVarRef (There ref)

type VR0 = Here
type VR1 = There Here
type VR2 = There VR1
type VR3 = There VR2
type VR4 = There VR3
type VR5 = There VR4


-- | Booleans

class IsBool ctor
data True
instance isBoolTrue
  :: IsBool True
data False
instance isBoolFalse
  :: IsBool False


-- | Expressions
-- | Terms in our language

class IsExpr ctor

data Var ref
instance isExprVar
  :: IsVarRef ref
  => IsExpr (Var ref)

type V0 = Var VR0
type V1 = Var VR1
type V2 = Var VR2
type V3 = Var VR3
type V4 = Var VR4
type V5 = Var VR5

data Lam body
instance isExprLam
  :: IsExpr body
  => IsExpr (Lam body)

data App fn arg
instance isExprApp
  :: (IsExpr fn, IsExpr arg)
  => IsExpr (App fn arg)

data Sym (sym :: Symbol)
instance isExprSym
  :: IsExpr (Sym sym)

data Cat left right
instance isExprCat
  :: (IsExpr left, IsExpr right)
  => IsExpr (Cat left right)

data Typ typ
instance isExprTyp
  :: IsExpr (Typ typ)

data Typ1 (typ1 :: * -> *)
instance isExprTyp1
  :: IsExpr (Typ1 typ1)

data Bool bool
instance isExprBool
  :: IsBool bool
  => IsExpr (Bool bool)

data If condition ifTrue ifFalse
instance isExprIf
  :: (IsExpr condition, IsExpr ifTrue, IsExpr ifFalse)
  => IsExpr (If condition ifTrue ifFalse)


-- | Values
-- | Results of evaluating an expression in a context

class IsValue ctor

data FunVal context body
instance isValueFunVal
  :: (IsContext context, IsExpr body)
  => IsValue (FunVal context body)

data SymVal (sym :: Symbol)
instance isValueSymVal
  :: IsValue (SymVal sym)

data TypVal typ
instance isValueTypVal
  :: IsValue (TypVal typ)

data Typ1Val (typ1 :: * -> *)
instance isValueTyp1Val
  :: IsValue (Typ1Val typ1)

data BoolVal bool
instance isValueBoolVal
  :: IsValue (BoolVal bool)


-- | Context
-- | A list of values bound by lambdas

class IsContext d

data Empty
instance contextEmpty
  :: IsContext Empty

data Extend value tail
instance contextExtend
  :: (IsValue value, IsContext tail)
  => IsContext (Extend value tail)
