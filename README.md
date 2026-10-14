
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stepcadence

<!-- badges: start -->

[![R-CMD-check](https://github.com/jhuwit/cadence/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/cadence/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jhuwit/cadence/graph/badge.svg)](https://app.codecov.io/gh/jhuwit/cadence)
<!-- badges: end -->

The goal of `stepcadence` is to calculate cadence (steps per minute)
metrics from minute-level step count data, following [Tudor-Locke et
al. (2018)](https://doi.org/10.1136/bjsports-2017-097628): peak and
maximum cadence, mean cadence, and time spent in cadence levels and
bands.

## Installation

You can install the development version of `stepcadence` from
[GitHub](https://github.com/jhuwit/cadence) with:

``` r
# install.packages("remotes")
remotes::install_github("jhuwit/cadence")
```

## Example

The input is a data frame with one row per minute and `time` and `steps`
columns. `summarize_cadence()` computes every metric, here for each
participant (`SEQN`) and day (`PAXDAYM`):

``` r
library(stepcadence)
head(simulated_nhanes_steps)
#>   SEQN PAXDAYM time steps
#> 1 ID_1       1    1     0
#> 2 ID_1       1    2     0
#> 3 ID_1       1    3     0
#> 4 ID_1       1    4     0
#> 5 ID_1       1    5     0
#> 6 ID_1       1    6     0

result = summarize_cadence(simulated_nhanes_steps,
                           group_cols = nhanes_group_cols())
result
#> # A tibble: 6 × 25
#>   SEQN  PAXDAYM peak_1min peak_30min peak_60min n_minutes n_minutes_wear
#>   <chr>   <int>     <dbl>      <dbl>      <dbl>     <int>          <int>
#> 1 ID_1        1       200        200        165      1440             NA
#> 2 ID_1        2       200        200        165      1440             NA
#> 3 ID_2        1       200        200        165      1440             NA
#> 4 ID_2        2       200        200        165      1440             NA
#> 5 ID_3        1       200        200        165      1440             NA
#> 6 ID_3        2       200        200        165      1440             NA
#> # ℹ 18 more variables: n_minutes_valid <int>, mean_uncensored_cadence <dbl>,
#> #   mean_censored_cadence <dbl>, max_5min <dbl>, max_20min <dbl>,
#> #   max_30min <dbl>, max_60min <dbl>, low_cadence <int>,
#> #   moderate_cadence <int>, high_cadence <int>, zero <int>, incidental <int>,
#> #   sporadic <int>, purposeful <int>, slow_walk <int>, medium_walk <int>,
#> #   brisk_walk <int>, faster_ambulation <int>
```

See `vignette("calculate_cadence", package = "stepcadence")` for details
on each metric, cadence bands, and handling non-wear time.
