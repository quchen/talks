QA playground
=============

This example project demonstrates basic usages of many important QA tools
available in the Haskell ecosystem.

- [x] Test framework [**Tasty**][tasty]
    - [x] Probabilistic batch testing with [**QuickCheck**][qc]
    - [x] Deterministic batch testing with [**Smallcheck**][sc]
    - [x] Example test case testing with [**HUnit**][hu]
    - [x] Deliberately failing tests to show reporting
- [x] Doctests with  [**Doctest**][doctest]
- [x] Generating coverage reports with [**HPC**][hpc]
- [x] Benchmarking with [**Criterion**][crit]
- [ ] Documentation with [**Haddock**][haddock]

Execute it with

```bash
stack build --test --coverage --benchmark
```



[crit]: http://hackage.haskell.org/package/criterion
[doctest]: http://hackage.haskell.org/package/doctest
[haddock]: https://www.haskell.org/haddock/
[hpc]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/hpc.html
[hu]: http://hackage.haskell.org/package/HUnit
[qc]: http://hackage.haskell.org/package/QuickCheck
[sc]: http://hackage.haskell.org/package/smallcheck
[tasty]: http://hackage.haskell.org/package/tasty