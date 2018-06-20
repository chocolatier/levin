# levin
SMT constraint analysis for grammar learning

## Setup

This project has the following unmanaged dependencies

- z3java - Add the jar to /lib/ and make sure that the DLLs are present in the path.

- kleaver - The easiest way to get this is building [KLEE](http://klee.github.io/build-llvm34/).

- S2E - This requires that you set up [S2E](https://github.com/S2E/docs/).

You also need to set up the configuration files in ```/src/main/resources/```.
Note that not all plugins in ```config.template.yml``` are available publicly. 


## Usage

The rapidly changing codebase is the only documentation.
