# projections 0.5.1

## Fixes and improvements

- now using the new default random number generator in tests

- updated tests to pass check with R 4.0.2; this includes re-generation of most
  reference objects, which were manually inspected and validated against old
  references


# projections 0.5.0

## New functions

- new function `merge_projections` permits to join different sets of
  simulations, adapting the respective time windows accordingly

- new function `merge_add_projections` permits to add simulated case incidence
  from different sets of simulations, adapting the respective time windows
  accordingly; if objects contain different numbers of simulations, the shortest
  ones are recycled as needed; also implemented as the operator `+`

- new function `summary` will summarise `projections` objects by day, using a
  range of pre-defined statistics (mean, sd, min, max, and user-defined
  quantiles)

## Fixes and improvements

- bug fix for the use of time-varying R in `project`

- bug fix for `as.data.frame(..., long = TRUE)`

- more consistent handling of inputs for the serial interval; if provided as a
  vector, `si` now starts at a delay of 1 day, rather than 0 and assuming the
  first entry is 0; for `distcrete` inputs, the mass for `si$d(0)` is now
  ignored, and the rest of the distribution is rescaled accorindly to ensure the
  PMF sums to 1

- (*documentation*) more realistic serial interval distribution used for the Ebola
  examples

- (*internal*) the force of infection is now calculated by the new internal
  function `compute_force_infection`
  
- (*testing*) revised sets of tests relying less on comparison to references,
  but testing for meaningful output properties instead
  
- (*testing*) 100% coverage


# projections 0.4.1

- `project()` will can now take single dates or single projections as inputs.
  (@acori, @zkamvar, #18).

# projections 0.4.0

- `project` can now use time-varying R by specifying `time_change`, a vector of
  dates at which R changes, and providing a `vector` or a `list` of values for
  `R` instead of a vector, in which case it needs to have `length(time_change) +
  1` components, each of which is a vector of R values.

# projections 0.3.2

- A bug in `project()` where R was being resampled recursively was fixed
  (#11, @jarvisc1; #12, @zkamvar)

# projections 0.3.1

 - `get_dates()` now inherits the generic `get_dates()` from incidence

# projections 0.3.0

## New features

This is a big release! Plenty of new features have been added, including:

- `projections` can now be subsetted like matrices using `x[i,j]`, or using the
  function `subset`
  
- new function `cumulate` to compute cumulative incidence for `projections`
  objects, akin to the similar function in the `incidence` package
  
- much improved graphics; `plot` now call upons `add_projections`, which
  implements many options for plotting projections, including quantiles lines,
  ribbon, and boxplots; `add_projections` can also be used to add such plots to
  an existing `incidence` plot

- `build_projections` can be used to build a `projections` object from an input
  matrix and optional dates



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

- `get_dates`: accessors for `projections` objects.

- `as.data.frame`: conversion from `projections` objects to `data.frame`.

