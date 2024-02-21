# Plutus Groth16 Validation

This repository contains a Groth 16 validation written in pure Plutus V2. This analysis is part of the Project Catalyst Fund 10 proposal "A Zero Knowledge Proof framework for Cardano based on Hydra and ZK-SNARKS". We aimed to use the unlimited execution budget of Hydra for having a Groth 16 validation and to demostrate the Plutus capabilities for this kind of tasks.

Having a Groth 16 validation in Plutus V2 is a challenging task, because the Plutus platform is not designed for this kind of tasks. The main chanllenge is the lack of support for elliptic curve operations in Plutus. As first step, we have implemented curve operations in the BLS12-381 curve and we had to implement multiple optimization techniques to make the validation feasible. The main optimization techniques are:

- Tower extension field arithmetic.
- Frobenius operator for exponentiation.
- Operations in Jacobian coordinates [algorithm catalog](https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html):
  - dbl-2009-l https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#doubling-dbl-2009-l
  - add-2007-bl https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl
- Optimal ate pairing. [High-Speed Software Implementation of the
  Optimal Ate Pairing over Barreto–Naehrig
  Curves](https://eprint.iacr.org/2010/354.pdf)

## How to use

We prepared a nix flake for building and running this project without difficulty. You can use the following commands to build and run the project:

```bash
$ nix develop

Restored session: Wed Feb 21 07:13:51 CET 2024

🤟 Welcome to nix-shell 🤟

Type 'info' to see what's inside this shell.

[nix-shell]$
```

## Build

Once you are in the nix-shell, you can build the project with the following command:

```bash
[nix-shell]$ cabal update
Downloading the latest package lists from:
- hackage.haskell.org
- cardano-haskell-packages
Package list of cardano-haskell-packages has been updated.
The index-state is set to 2024-02-15T15:59:55Z.
To revert to previous state run:
    cabal v2-update 'cardano-haskell-packages,2023-12-01T11:57:23Z'
Package list of hackage.haskell.org has been updated.
The index-state is set to 2024-02-21T05:21:00Z.
To revert to previous state run:
    cabal v2-update 'hackage.haskell.org,2023-12-02T06:48:51Z'

[nix-shell]$ cabal build all
Warning: The package list for 'hackage.haskell.org' is 80 days old.
Run 'cabal update' to get the latest list of available packages.
Warning: The package list for 'cardano-haskell-packages' is 80 days old.
Run 'cabal update' to get the latest list of available packages.
Resolving dependencies...
Build profile: -w ghc-9.6.3 -O1
In order, the following will be built (use -v for more details):
 - plutus-groth-0.1.0.0 (lib) (first run)
 - plutus-groth-0.1.0.0 (exe:plutus-groth) (first run)
Configuring library for plutus-groth-0.1.0.0..
Preprocessing library for plutus-groth-0.1.0.0..
Building library for plutus-groth-0.1.0.0..
[1 of 2] Compiling BLS12381         ( src/BLS12381.hs, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/build/BLS12381.o, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/build/BLS12381.dyn_o )
[2 of 2] Compiling Groth16          ( src/Groth16.hs, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/build/Groth16.o, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/build/Groth16.dyn_o )
Configuring executable 'plutus-groth' for plutus-groth-0.1.0.0..
Preprocessing executable 'plutus-groth' for plutus-groth-0.1.0.0..
Building executable 'plutus-groth' for plutus-groth-0.1.0.0..
[1 of 2] Compiling Groth16Plc       ( app/Groth16Plc.hs, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/x/plutus-groth/build/plutus-groth/plutus-groth-tmp/Groth16Plc.o )
[2 of 2] Compiling Main             ( app/Main.hs, /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/x/plutus-groth/build/plutus-groth/plutus-groth-tmp/Main.o )
[3 of 3] Linking /Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/x/plutus-groth/build/plutus-groth/plutus-groth
```

The compalition requires a lot of time and resources. We recommend to have at least 16GB of RAM and 8 cores for the compilation. It could take some minutes to compile the project.

# Project Catalyst Fund 10 proposal

The objetive of this project in the Project Catalyst scope is to get a valid Groth 16 verification with some unit tests and get a budget benchmark for the execution of the validation. This benchmark will be used to compare the execution budget with other options.

## Test

Once you succesfully compiled the project, you can run the tests with the following command:

```bash
[nix-shell]$ cabal test
Build profile: -w ghc-9.6.3 -O1
In order, the following will be built (use -v for more details):
 - plutus-groth-0.1.0.0 (test:plutus-groth-test) (first run)
Preprocessing test suite 'plutus-groth-test' for plutus-groth-0.1.0.0..
Building test suite 'plutus-groth-test' for plutus-groth-0.1.0.0..
Running 1 test suites...
Test suite plutus-groth-test: RUNNING...
Test suite plutus-groth-test: PASS
Test suite logged to:
/Users/juan.magan/ws/Modulo-P/plutus-groth/dist-newstyle/build/x86_64-osx/ghc-9.6.3/plutus-groth-0.1.0.0/t/plutus-groth-test/test/plutus-groth-0.1.0.0-plutus-groth-test.log
1 of 1 test suites (1 of 1 test cases) passed.
```

The file `test/Groth16Test.hs`contains a basic validation tests. We find the verification key and the proof. We play with the public instance for passing or failing the validation. It should pass only with the integer 168932 and so it's demonstrated in the test.

## Budget benchmark

We have prepared a benchmark for the execution of the validation.

```bash
[nix-shell:~/ws/Modulo-P/plutus-groth]$ cabal run plutus-groth
Plutus Core Groth16 benchmarks
===============================

Single pairing benchmark
CPU: 10000000000 MEM: 14000000 (Maximum budget)
CPU: 362874651295 MEM: 443016679 (Actual costs)

Point multiplication benchmark
CPU: 10000000000 MEM: 14000000 (Maximum budget)
CPU: 170244281089 MEM: 424413813 (Actual costs)

Complete proof benchmark
CPU: 10000000000 MEM: 14000000 (Maximum budget)
CPU: 1334647992336 MEM: 1663887424 (Actual costs)
```

As we can see, the actual costs exceed the maximum budget. This was expected, because this validator is designed to run on Hydra, where the execution budget is unlimited. We will use this benchmark to compare the execution budget with other options.

## Conclusion

This validator proof the concept of having a Groth 16 validation in Plutus V2. We have implemented the curve operations and the optimal ate pairing. We have also implemented multiple optimization techniques to make the validation feasible. We have also prepared a budget benchmark for the execution of the validation. This benchmark will be used to compare the execution budget with other options.

The IOG's Plutus Team has developed the Plutus V3 which includes the support for the BLS12-381 curve operations. The builtin primitives for the BLS12-381 curve operations will make the Groth 16 validation feasible in Plutus V3 with a reasonable execution budget.

As a result of this study, we left this repository as a proof of concept and we will continue the development of the Groth 16 validation in Plutus V3.
