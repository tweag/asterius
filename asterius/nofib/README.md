
# NoFib: Haskell Benchmark Suite (GEORGE's NOTES)

This folder contains a selection of tests taken from [GHC's `nofib` testsuite](https://gitlab.haskell.org/ghc/nofib).

## Changelog

Differences compared to the original `nofib`:

* We do not use any symlinks (we replicate files instead). Easier to reproduce.
* The `gc`, `parallel`, and `smp` subdirectories are omitted.
* Every test lives at the same level (arbitrary nesting complicates things for no benefit).
* We do not use a Makefile-based system to build the tests.
* Removed test `shootout/reverse-complement` (too big inputs and outputs, used c program to generate them)

## Executing the Tests
To remove all material from previous executions, just run:
```bash
python3 script.py clean
```

To run the tests, type the following:
```bash
python3 script.py <MODE>
```
where `<MODE>` is one of `fast`, `norm`, or `slow`. This should create a `TIMES.txt` file, whose content is in the following CSV format:
```
category, testname, compiler, mode, duration (in seconds)
```
For example, it contains entries like the following:
```
imaginary,primes,ghc,fast,0.23458600044250488
imaginary,primes,ahc,fast,1.422548770904541
imaginary,tak,ghc,fast,0.08010053634643555
imaginary,tak,ahc,fast,1.0395824909210205
...
```

## Current Failures

### Seem to run forever
```
real/eff-S
shootout/pidigits
```

### JavaScript heap out of memory
```
real/pic
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory

real/scs
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory

shootout/fannkuch-redux
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory

shootout/k-nucleotide
  FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
```

