# ggdnog

<!-- badges: start -->
<!-- badges: end -->

The goal of ggdnog is to provide some helpful functions for daily use at a GGD.

## Installation

You can install the development version of ggdnog from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aartdijkstra/ggdnog")
```

## Useful functions

Some basic helpers are:
- `printf()` - wrapper for `sprintf()` which outputs the result
- `freq.table()` - combines the functionality of `table()` and `proportions()` into a human-readable frequency table with included proportions and chi^2-testing
- `describe()` - combines the functionality of `summary()` and `aggregate` into a human-readable summary of data structures. Characters and factors are automatically presented in a table, and the missing parameters from `summary()` are added for numerical types.
- `dir_maps()` - automatically traverses the directory tree searching for a specific location with maps
- `scale_value()` and `scale_axis()` - allow for easy rescaling of results for a secondary y-axis in ggplot


``` r
library(ggdnog)

printf("This allows for easy printing of numbers (%d, %.1f) and other types (%s).",
       5, 0.24, "like this string")
       
data = data.frame(weight = rnorm(100, mean=70, sd=15),
                  gender = c(rep("male", 50), rep("female", 50)))
describe(data, "gender")
describe(data$weight, data$gender)
```

