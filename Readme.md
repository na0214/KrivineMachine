# Krivine Machine

An implementation of Krivine Machine that introduced by the "A Core Quantitative Coeffect Calculus".

# Installation

```bash
$ git clone https://github.com/na0214/KrivineMachine.git
$ cd KrivineMachine
$ dune build
```

# Usage

```bash
_build/default/krivine.exe
```

If you want to use a custom coeffect handler,please change the definition of module Coeff in the krivine.ml.

As well,if you want to evaluate a custom expression,please change the definition of exp in the krivine.ml.