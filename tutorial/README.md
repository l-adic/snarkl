# Tutorial

This tutorial contains the following examples

- [composite-test](./composite-test/Main.md) (easy)
- [sudoku](./sudoku/Sudoku.md) (advanced)

For other examples of user defined data types and some programs, you can check out the Snarkl [examples](../examples/Snarkl/Example/) folder.

## Notes

### RebindableSyntax Extension etc.

Often Snarkl programs are written with the `{-# LANGUAGE RebindableSyntax #-}` extension. This allows you to override the normal `do` syntax to express programs in an imperative style and is extremely convenient. The downside to this is that e.g. normal `if-then-else` syntax for native Haskell `Bool` values is disabled, as is any association of `do` syntax with the normal haskell `Monad` typeclass. This is why you may also need disable some warnings and `hlint` suggestions that go with this.

There is the other caveat that many functions exported from `Snarkl.Language.Prelude` etc take the same name as their corresponding definitions from the standard Haskell `Prelude`. It's more convenient to enable `NoImplicitPrelude` and selectively import elements into your Snarkl program.

In short, most Snarkl programs will use the following intro pragmas to keep you and your text editor happy.

```haskell
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}
```

### Types With "Tick Marks"

You will see types throughout Snarkl that start with a "tick mark", e.g. `'TField`, `'TBool` etc. This is an unfortunate implementation detail that is related to how Snarkl uses the `DataKinds` language extension. At the time of writing this tutorial, there is no excellent way to deal with this -- even if we were to introduce type aliases to get around them, the originals with tick marks would still appear in compiler error messages.

Certain types like `Vector` and `Matrix` have their own type aliases for sanity sake. Other user defined types also have them.

### Galois Fields

The constraint `GaloisField k` appears regularly throughout the codebase and in the examples, as well as the subclass `PrimeField k`. The distinction is:
- `PrimeField k`: `k` is a finite field of prime order (usually `F_BN128`, which is the field of definition of the `BN_128` curve)
- `GaloisField k`: `k` is a finite separable extension of a `PimeField`.

For `PrimeField`  the serialzation format is the same as `Natural` or `Integer`. At the time of writing this tutorial, there is no canonical serialization 
for `GaloisField` as this would require speciying the basis order.

### arkworks-bridge

Snarkl is used to arithmetize programs, ultimately compiling programs to an r1cs and solve for a witness. It cannot do other necessary steps in generating zk snarks, such as creating a trusted setup, generating proofs, or verifying them. For these steps you can use [arkworks-bridge](https://github.com/l-adic/arkworks-bridge) which is designed to pick up where Snarkl leaves off.
