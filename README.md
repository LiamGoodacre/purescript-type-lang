# Lambda calculus in PureScript types

Check at the bottom of [/test/Main.purs](/test/Main.purs) for examples.


## Expressions

* String literals via symbols: `Sym "example"`
* String concatenation: `Cat (Sym "Hello") (Sym "World")`
* Boolean literals: `Bool True`/`Bool False`
* If expressions: `If (Bool True) (Sym "X") (Sym "Y")` to get `Sym "X"`
* Type literals: `Typ Int`, `Typ (Maybe Int)`
* Type constructor literals: `Typ1 Maybe`, `Typ1 Array`
* Functions via `Lam :: * -> *`:
  * `Lam (Sym "example")` is a function returning the symbol `"example"`
* Variables in binder counting form:
  * `Var Here`/`V0`
  * `Var (There Here)`/`V1`
  * ...
  * `V5`
* Application via `App f x`:
  * Type constructors: `App (Typ1 Array) (Typ Int)` to get `Typ (Array Int)`
  * Functions: `App (Lam V0) (Sym "x")` to get `Sym "x"`
* Empty record via `EmptyRec`
* Record cons via `ConsRec k v r`
* Record index via `IndexRec k r`


## Values

* Functions
* Symbols
* Types
* Type constructors
* Booleans
* Records
