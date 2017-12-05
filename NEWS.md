# projections 0.0.1

First release of the package!


## New features

- `project`: a function generating projections from an existing *incidence*
  object, a serial interval distribution, and a set of plausible reproduction
  numbers ($R$); returns a `projections` object.
  
- `plot`/`print`: plotting and printing methods for `projections` objects.

- `get_dates`/`get_incidence`: accessors for `projections` objects.

- `as.data.frame`: conversion from `projections` objects to `data.frame`.

