# wtsGMACS

This is an R package to deal with GMACS assessment data and results.

## Installation

This R package requires the following R packages be installed (links are to repositories):

  * tcsamFunctions [https://github.com/wStockhausen/tcsamFunctions]
  * wtsPlots [https://github.com/wStockhausen/wtsPlots]
  * wtsUtilities [https://github.com/wStockhausen/wtsUtilities]

The packages above can be installed using

`devtools::install_github("wStockhausen/pkg_name")`

where `pkg_name` is the name of the package above. To avoid error messages when installing 
the packages, install them in the reverse order listed above.

Other required packages available on CRAN include:

  * dplyr
  * ggplot2 
  * graphics 
  * magrittr 
  * RColorBrewer 
  * readr 
  * reshape2 
  * stats 
  * tibble 
  * tidyr

## How to use 

The `runGMACS` function can be used to run a GMACS model, as in:

`wtsGMACS::runGMACS("testing/example_run")`

Use `?wtsGMACS::runGMACS` for more information.

The `runJitter` function can be used to evaluate a series of model runs under parameter 
jittering. Use `?wtsGMACS::runJitter` for more information.

The `readModelResults` function can be used to read results from the `par`, `std`, 
`Gmacsall.std`, and `gmacs.rep1` files for multiple model runs:

```
fldrs_ = c(A="testing/example_results",
           B="testing/example_results");
Ms = wtsGMACS::readModelResults(fldrs_);
```

Use `?wtsGMACS::readModelResults` for more information.