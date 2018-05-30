[![Build Status](https://travis-ci.org/reconhub/projections.svg?branch=master)](https://travis-ci.org/reconhub/projections)
[![Build status](https://ci.appveyor.com/api/projects/status/265h2el4y9popan9/branch/master?svg=true)](https://ci.appveyor.com/project/thibautjombart/projections/branch/master)
[![codecov.io](https://codecov.io/github/reconhub/projections/coverage.svg?branch=master)](https://codecov.io/github/reconhub/projections?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/projections)](https://cran.r-project.org/package=projections)





# Welcome to the *projections* package!

This package uses data on *daily incidence*, the *serial interval* (time between
onsets of infectors and infectees) and the *reproduction number* to simulate
plausible epidemic trajectories and project future incidence. It relies on a
branching process where daily incidence follows a Poisson process determined by
a daily infectiousness, computed as:

$$
\lambda_t = \sum_{s = 1}^{t - 1} y_s w(t - s)
$$

where $w()$ is the probability mass function (PMF) of the serial interval, and
$y_s$ is the incidence at time $s$.


## Installing the package

To install the current stable, CRAN version of the package, type:

```r
install.packages("projections")
```

To benefit from the latest features and bug fixes, install the development, *github* version of the package using:

```r
devtools::install_github("reconhub/projections")
```

Note that this requires the package *devtools* installed.


# What does it do?

The main features of the package include:

- **`project`**: a function generating projections from an existing *incidence*
  object, a serial interval distribution, and a set of plausible reproduction
  numbers ($R$); returns a `projections` object.
  
- **`plot`/`print`**: plotting and printing methods for `projections` objects.

- **`get_dates`**: accessors for `projections` objects.

- **`cumulate`**: cumulate predicted incidence over time.

- **`as.data.frame`**: conversion from `projections` objects to `data.frame`.

- **`[`**: subsetting operator for *projections* objects, permiting to specify
  which dates and simulations to retain; uses a syntax similar to matrices,
  i.e. `x[i, j]`, where `x` is the *projections* object, `i` a subset of dates,
  and `j` a subset of simulations.

- **`subset`**: subset an *projections* object by specifying a time window.


# Resources

## Worked example

In the following, we project future incidence using a simulate Ebola outbreak
distributed in the package `outbreaks`:

```r
library(outbreaks)
library(incidence)

onset <- ebola_sim$linelist$date_of_onset
i <- incidence(onset)
plot(i) # full outbreak
```

<img src="figure/onset-1.png" title="plot of chunk onset" alt="plot of chunk onset" width="80%" />

```r
plot(i[1:160]) # first 160 days
```

<img src="figure/onset-2.png" title="plot of chunk onset" alt="plot of chunk onset" width="80%" />

We create a serial interval distribution using `distcrete`:

```r
library(distcrete)
library(epitrix)
mu <- 15.3
sigma <- 9.3
cv <- sigma / mu
params <- gamma_mucv2shapescale(mu, cv)
params
```

```
## $shape
## [1] 2.706556
## 
## $scale
## [1] 5.652941
```

```r
si <- distcrete("gamma", shape = params$shape,
                scale = params$scale,
                interval = 1, w = 0)
si
```

```
## A discrete distribution
##   name: gamma
##   parameters:
##     shape: 2.70655567117586
##     scale: 5.65294117647059
```

```r
plot(1:50, si$d(1:50), type = "h", lwd = 3, col = "navy",
     main = "Serial interval", xlab = "Days after onset",
     ylab = "Relative infectiousness")
```

<img src="figure/si-1.png" title="plot of chunk si" alt="plot of chunk si" width="80%" />


We predict future incidence based on these data, assuming a reproduction number
of 1.5, from day 100 and for 60 days:


```r
library(projections)
set.seed(1)
pred <- project(i[1:100], R = 1.5, si = si, n_days = 60, n_sim = 1000)
pred
```

```
## 
## /// Incidence projections //
## 
##   // class: projections, matrix
##   // 60 dates (rows); 1,000 simulations (columns)
## 
##  // first rows/columns:
##            [,1] [,2] [,3] [,4] [,5] [,6]
## 2014-07-16    7    8    6   12    4    3
## 2014-07-17   10    7    5    7   10   12
## 2014-07-18    3    6    6   11    6    6
## 2014-07-19    9    7    9    9    6   12
##  .
##  .
##  .
## 
##  // dates:
##  [1] "2014-07-16" "2014-07-17" "2014-07-18" "2014-07-19" "2014-07-20"
##  [6] "2014-07-21" "2014-07-22" "2014-07-23" "2014-07-24" "2014-07-25"
## [11] "2014-07-26" "2014-07-27" "2014-07-28" "2014-07-29" "2014-07-30"
## [16] "2014-07-31" "2014-08-01" "2014-08-02" "2014-08-03" "2014-08-04"
## [21] "2014-08-05" "2014-08-06" "2014-08-07" "2014-08-08" "2014-08-09"
## [26] "2014-08-10" "2014-08-11" "2014-08-12" "2014-08-13" "2014-08-14"
## [31] "2014-08-15" "2014-08-16" "2014-08-17" "2014-08-18" "2014-08-19"
## [36] "2014-08-20" "2014-08-21" "2014-08-22" "2014-08-23" "2014-08-24"
## [41] "2014-08-25" "2014-08-26" "2014-08-27" "2014-08-28" "2014-08-29"
## [46] "2014-08-30" "2014-08-31" "2014-09-01" "2014-09-02" "2014-09-03"
## [51] "2014-09-04" "2014-09-05" "2014-09-06" "2014-09-07" "2014-09-08"
## [56] "2014-09-09" "2014-09-10" "2014-09-11" "2014-09-12" "2014-09-13"
```

```r
plot(pred) # default plot
```

<img src="figure/predictions-1.png" title="plot of chunk predictions" alt="plot of chunk predictions" width="80%" />

```r
pred_cum <- cumulate(pred) # cumulative predictions
plot(pred_cum) # plot cumulative predictions
```

<img src="figure/predictions-2.png" title="plot of chunk predictions" alt="plot of chunk predictions" width="80%" />

```r
apply(pred, 1, mean) # average prediction per day
```

```
## 2014-07-16 2014-07-17 2014-07-18 2014-07-19 2014-07-20 2014-07-21 
##      6.912      7.289      7.706      7.774      8.098      8.425 
## 2014-07-22 2014-07-23 2014-07-24 2014-07-25 2014-07-26 2014-07-27 
##      8.669      9.112      9.272      9.712     10.003     10.024 
## 2014-07-28 2014-07-29 2014-07-30 2014-07-31 2014-08-01 2014-08-02 
##     10.477     10.943     11.320     11.410     12.073     12.162 
## 2014-08-03 2014-08-04 2014-08-05 2014-08-06 2014-08-07 2014-08-08 
##     12.543     13.035     13.437     13.667     14.265     14.718 
## 2014-08-09 2014-08-10 2014-08-11 2014-08-12 2014-08-13 2014-08-14 
##     14.908     15.553     16.114     16.328     16.844     17.398 
## 2014-08-15 2014-08-16 2014-08-17 2014-08-18 2014-08-19 2014-08-20 
##     17.880     18.263     18.995     19.770     20.108     20.817 
## 2014-08-21 2014-08-22 2014-08-23 2014-08-24 2014-08-25 2014-08-26 
##     21.260     22.123     22.876     23.247     23.956     24.795 
## 2014-08-27 2014-08-28 2014-08-29 2014-08-30 2014-08-31 2014-09-01 
##     25.603     26.175     27.145     27.997     28.709     29.856 
## 2014-09-02 2014-09-03 2014-09-04 2014-09-05 2014-09-06 2014-09-07 
##     30.484     31.232     32.390     33.385     34.336     34.978 
## 2014-09-08 2014-09-09 2014-09-10 2014-09-11 2014-09-12 2014-09-13 
##     36.270     37.390     38.474     39.556     41.200     42.389
```

```r
apply(pred, 1, range) # range across simulations
```

```
##      2014-07-16 2014-07-17 2014-07-18 2014-07-19 2014-07-20 2014-07-21
## [1,]          0          1          1          0          1          1
## [2,]         16         17         17         17         17         18
##      2014-07-22 2014-07-23 2014-07-24 2014-07-25 2014-07-26 2014-07-27
## [1,]          1          2          1          2          2          2
## [2,]         21         19         22         21         20         23
##      2014-07-28 2014-07-29 2014-07-30 2014-07-31 2014-08-01 2014-08-02
## [1,]          3          2          2          2          3          2
## [2,]         22         25         25         27         26         27
##      2014-08-03 2014-08-04 2014-08-05 2014-08-06 2014-08-07 2014-08-08
## [1,]          3          3          3          2          4          4
## [2,]         26         28         25         28         31         29
##      2014-08-09 2014-08-10 2014-08-11 2014-08-12 2014-08-13 2014-08-14
## [1,]          4          5          4          4          6          5
## [2,]         30         33         35         31         36         32
##      2014-08-15 2014-08-16 2014-08-17 2014-08-18 2014-08-19 2014-08-20
## [1,]          5          5          6          7          6          8
## [2,]         37         36         40         38         38         39
##      2014-08-21 2014-08-22 2014-08-23 2014-08-24 2014-08-25 2014-08-26
## [1,]          8          7          6          6          6          8
## [2,]         40         42         49         44         48         48
##      2014-08-27 2014-08-28 2014-08-29 2014-08-30 2014-08-31 2014-09-01
## [1,]          9         10         10          9         10         12
## [2,]         46         45         49         51         51         55
##      2014-09-02 2014-09-03 2014-09-04 2014-09-05 2014-09-06 2014-09-07
## [1,]         12         11         13         14         14         11
## [2,]         65         62         58         69         60         68
##      2014-09-08 2014-09-09 2014-09-10 2014-09-11 2014-09-12 2014-09-13
## [1,]         14         14         16         12         19         21
## [2,]         66         68         63         71         75         75
```

An alternative representation of the outcomes:

```r
library(ggplot2)
df <- as.data.frame(pred, long = TRUE)
head(df)
```

```
##         date incidence sim
## 1 2014-07-16         7   1
## 2 2014-07-17        10   1
## 3 2014-07-18         3   1
## 4 2014-07-19         9   1
## 5 2014-07-20        13   1
## 6 2014-07-21         5   1
```

```r
p <- ggplot(df, aes(x = date, y = incidence)) +
  geom_jitter(alpha = .3) + geom_smooth()
p
```

```
## `geom_smooth()` using method = 'gam'
```

<img src="figure/plots-1.png" title="plot of chunk plots" alt="plot of chunk plots" width="80%" />


Predictions can also be added to the epicurve:

```r
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following objects are masked from 'package:testthat':
## 
##     equals, is_less_than, not
```

```r
plot(i[20:160]) %>% add_projections(pred, boxplots = FALSE)
```

<img src="figure/plot_with_incidence-1.png" title="plot of chunk plot_with_incidence" alt="plot of chunk plot_with_incidence" width="80%" />




## Vignettes

*projections* does not currently have a dedicated vignette; instead, it is
illustrated in conjunction with `earlyR` on [this
vignette](http://www.repidemicsconsortium.org/earlyR/articles/earlyR.html).


## Websites

A dedicated website can be found at:
[http://www.repidemicsconsortium.org/projections](http://www.repidemicsconsortium.org/projections).






## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue*](http://github.com/reconhub/projections/issues) system. All other
questions should be posted on the **RECON forum**: <br>
[http://www.repidemicsconsortium.org/forum/](http://www.repidemicsconsortium.org/forum/)

Contributions are welcome via [pull
requests](https://github.com/reconhub/projections/pulls).

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to abide by its
terms.

