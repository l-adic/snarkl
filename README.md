# Snårkl 

![CI](https://github.com/l-adic/snarkl/actions/workflows/nix-ci.yml/badge.svg)


Snårkl ("Snorkel") is a high-level language and compiler for verifiable computing. It is capable of compiling an embedded DSL to a system of arithmetic constraints and solving for a satisfying witness. These programs can be embedded in larger Haskell programs or used as standalone applications via the CLI bindings.

## Tutorial

There is a [tutorial](./tutorial/README.md) which is written in literate Haskell (i.e. it compiles to an executable). You can also find some interesting [examples](./examples/Snarkl/Example/) involving user defined datatypes.

## zk-SNARKs
Snarkl can be used either for simple verified computation or for zk-SNARK production. However, Snarkl is not capable of generating zk-SNARKs by itself -- it is only able to compile programs to a system of arithmetic constraints (e.g. an R1CS), or to solve for a witness for these constraints. It is possible to include both public and private input data to these computations, but Snarkl cannot e.g. create a trusted setup, construct proofs, or verify them.

For this reason we have written an integration with the [arkworks](https://github.com/arkworks-rs/) framework called [arkworks-bridge](https://github.com/l-adic/arkworks-bridge). Snarkl can serialize it's constraint and witness values to a [JSON lines](https://jsonlines.org/) format which is compatible with `arkworks-bridge`. You can use `arkworks-bridge` as a CLI program or embed it in a larger rust program.

We provide an [example application](https://github.com/l-adic/purescript-zk) which uses `arkworks-bridge` to generate a verifying contract on Ethereum for your Snarkl programs. It also shows how to construct and submit proofs to this verifier.

## Attributions

This repository is a fork/continuation of the [original](https://github.com/gstew5/snarkl) which is no longer actively developed. The original authors also wrote a paper describing the original implementation which you can find [here](http://ace.cs.ohio.edu/~gstewart/papers/snaarkl.pdf). They did amazing work and deserve most of the credit for what's here.
