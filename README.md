# extract-gating-ml


## Building

I built this project in a computing environment with 
 * GCC version 11.2.0

The computing environment was using environment modules (using LMod 8.3.10, https://github.com/TACC/Lmod) :
 * OpenBLAS/0.3.18-GCC-11.2.0
 * GMP/6.2.1-GCCcore-11.2.0
 * expat/2.4.1-GCCcore-11.2.0
 * bzip2/1.0.8-GCCcore-11.2.0

where I could run 
```bash
module load OpenBLAS/0.3.18-GCC-11.2.0
module load GMP/6.2.1-GCCcore-11.2.0
module load expat/2.4.1-GCCcore-11.2.0
module load bzip2/1.0.8-GCCcore-11.2.0
```

You will also need the Haskell stack build tool, see installation instructions at:
 * https://docs.haskellstack.org/en/stable/README/#how-to-install


After all that is installed, then run
 * stack build
 * stack install
