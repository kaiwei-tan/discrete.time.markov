
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discrete.time.markov

<!-- badges: start -->
<!-- badges: end -->

Applies the discrete time Markov model (Markov chain) to analyze data by
computing probabilties of transitions between different states.

Inspired by my Stochastic Models in Management module, as well as my
Honors Dissertation, taken in the National University of Singapore (NUS)
Business School.

## Installation

You can install the released version of discrete.time.markov from my
[GitHub](https://github.com/kaiwei-tan/discrete.time.markov) with:

``` r
library(devtools)
install_github("kaiwei-tan/discrete.time.markov")
```

## Example

This is a basic example, where we generate a random sequence of states
and calculate their transition probabilities:

``` r
library(discrete.time.markov)
# Generate random sequence of states
set.seed(57)
states <- sample(c('up', 'down', 'same'), 10000, replace=TRUE)
# Create transition probability matrix
get.transition.matrix(states, option='prob', output_type='matrix')
#>           down      same        up
#> down 0.3348044 0.3309797 0.3342159
#> same 0.3324275 0.3324275 0.3351449
#> up   0.3524939 0.3302920 0.3172141
```
