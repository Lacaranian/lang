# Lang

An exercise in programming language design.

Currently figuring out what it wants to be, but eventually this aspires to be a toy programming language compiler.

### Design goals

- Zero-runtime cost abstractions
- Functional/declarative first, imperative/mutable second
    - Monadic side effects
- OO via Algebraic Data Types and a typeclass/trait system
- Type System (wishlist of things to explore, these may conflict with each other)
    - Calculus of Constructions
    - Parametric polymorphism
    - Row polymorphism (polymorphism by composition)
    - Dependent types/quantification
    - Cumulative subtyping
    - Enough constraints to make type inference decidable
- A quiet prelude with just constructs used for most programs
    - Common data types/collections (Unit, Nat, Option, List, Map)
    - Category theoretical typeclasses
- Compile to other languages or IR to let them get native codegen