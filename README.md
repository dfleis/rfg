# Random Function Generator

Implementation of the "random function generator" used in Friedman (2001) for synthetic data experiments.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Usage](#usage)
4. [TODO](#todo)
5. [References](#references)

## Introduction

TODO

## Installation

```R
devtools::install_github("dfleis/rfg")
```

## Usage

TODO

## TODO

* Replace the `incl.params` mechanism in `rfg()` with an optional user-supplied environment via some new argument `env = NULL`. If `incl.params = TRUE` then `rfg()` adds the generated parameters as attributes on the function. Instead, I think it would be preferable to store the parameters in some environment such that advanced users could be able to modify the parameters after generation-time.

## References
Jerome H. Friedman.
<b>Greedy function approximation: A gradient boosting machine.</b>
<i>The Annals of Statistics</i>, 29, 2001.
[<a href="https://doi.org/10.1214/aos/1013203451">paper</a>]
