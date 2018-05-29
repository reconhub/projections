# projections 0.1.1

## New features

- `project` can now use a Negative Binomial model as an alternative to the
  original Poisson model to account for over-dispersion in new cases /
  super-spreading.


## Bug fixes

Fixed the calls to `sample`, which has a rather irrational behaviour when
passing a first argument of length 1 (i.e. not bootstrapping from the vector).




# projections 0.0.1

First release of the package!


## New features

- `project`: a function generating projections from an existing *incidence*
  object, a serial interval distribution, and a set of plausible reproduction
  numbers ($R$); returns a `projections` object.
  
- `plot`/`print`: plotting and printing methods for `projections` objects.

- `get_dates`/`get_incidence`: accessors for `projections` objects.

- `as.data.frame`: conversion from `projections` objects to `data.frame`.

