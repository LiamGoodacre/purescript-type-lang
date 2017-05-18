module Type.Lang.Types where

-- | Variable references, currently equivalent to natural numbers.
class IsVarRef ctor

data Here = Here
instance isVarRefHere
  :: IsVarRef Here

data There ref = There ref
instance isVarRefThere
  :: IsVarRef ref
  => IsVarRef (There ref)

type VR0 = Here
vr0 :: VR0
vr0 = Here

type VR1 = There VR0
vr1 :: VR1
vr1 = There vr0

type VR2 = There VR1
vr2 :: VR2
vr2 = There vr1

type VR3 = There VR2
vr3 :: VR3
vr3 = There vr2

type VR4 = There VR3
vr4 :: VR4
vr4 = There vr3

type VR5 = There VR4
vr5 :: VR5
vr5 = There vr4


-- | Booleans

class IsBool ctor
data True = True
instance isBoolTrue
  :: IsBool True
data False = False
instance isBoolFalse
  :: IsBool False


-- | Expressions
-- | Terms in our language

class IsExpr ctor

data Var ref = Var ref
instance isExprVar
  :: IsVarRef ref
  => IsExpr (Var ref)

type V0 = Var VR0
v0 :: V0
v0 = Var vr0

type V1 = Var VR1
v1 :: V1
v1 = Var vr1

type V2 = Var VR2
v2 :: V2
v2 = Var vr2

type V3 = Var VR3
v3 :: V3
v3 = Var vr3

type V4 = Var VR4
v4 :: V4
v4 = Var vr4

type V5 = Var VR5
v5 :: V5
v5 = Var vr5

data Lam body = Lam body
instance isExprLam
  :: IsExpr body
  => IsExpr (Lam body)

data App fn arg = App fn arg
instance isExprApp
  :: (IsExpr fn, IsExpr arg)
  => IsExpr (App fn arg)

data Sym (sym :: Symbol) = Sym
instance isExprSym
  :: IsExpr (Sym sym)

data Cat left right = Cat left right
instance isExprCat
  :: (IsExpr left, IsExpr right)
  => IsExpr (Cat left right)

data Typ typ = Typ
instance isExprTyp
  :: IsExpr (Typ typ)

data Typ1 (typ1 :: Type -> Type) = Typ1
instance isExprTyp1
  :: IsExpr (Typ1 typ1)

data Bool bool = Bool bool
instance isExprBool
  :: IsBool bool
  => IsExpr (Bool bool)

data If condition ifTrue ifFalse = If condition ifTrue ifFalse
instance isExprIf
  :: (IsExpr condition, IsExpr ifTrue, IsExpr ifFalse)
  => IsExpr (If condition ifTrue ifFalse)

data EmptyRec = EmptyRec
instance isExprEmptyRec
  :: IsExpr EmptyRec

data ConsRec (key :: Type) (val :: Type) (tail :: Type) = ConsRec
instance isExprConsRec
  :: IsExpr tail
  => IsExpr (ConsRec key val tail)

data IndexRec (key :: Type) (rec :: Type) = IndexRec
instance isExprIndexRec
  :: IsExpr rec
  => IsExpr (IndexRec key rec)


-- | Values
-- | Results of evaluating an expression in a context

class IsValue ctor

data FunVal context body = FunVal context body
instance isValueFunVal
  :: (IsContext context, IsExpr body)
  => IsValue (FunVal context body)

data SymVal (sym :: Symbol) = SymVal
instance isValueSymVal
  :: IsValue (SymVal sym)

data TypVal typ = TypVal typ
instance isValueTypVal
  :: IsValue (TypVal typ)

data Typ1Val (typ1 :: Type -> Type) = Typ1Val
instance isValueTyp1Val
  :: IsValue (Typ1Val typ1)

data BoolVal bool = BoolVal bool
instance isValueBoolVal
  :: IsValue (BoolVal bool)

data RecVal (rec :: # Type) = RecVal (Record rec)
instance isValueRecVal
  :: IsValue (RecVal rec)


-- | Context
-- | A list of values bound by lambdas

class IsContext d

data Empty = Empty
instance contextEmpty
  :: IsContext Empty

data Extend value tail = Extend value tail
instance contextExtend
  :: (IsValue value, IsContext tail)
  => IsContext (Extend value tail)
