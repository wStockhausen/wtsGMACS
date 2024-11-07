# wtsGMACS

This is an R package to deal with GMACS assessment data and results.

## Installation

After installing the required packages (see below), this package can be installed using:

```
devtools::install_github("wStockhausen/wtsGMACS")
```

### Dependencies

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

```
wtsGMACS::runGMACS(runpath=path_to_run_folder,
                   runCmds= list(path2exe=path_to_exe,
                                 path2dat=path_to_gmacs_dat_file);
````
In the above, `path_to_run_folder` is a path (absolute or relative to the R working directory) to the folder 
in which you want the model to run, `path_to_exe` is a path to 
the gmacs executable, and `path_to_gmacs_dat_file` is a path to the `gmacs.dat` file describing 
the model run, 

Use `?wtsGMACS::runGMACS` for more information.

The `runJitter` function can be used to evaluate a series of model runs under parameter 
jittering. Use `?wtsGMACS::runJitter` for more information.

The `readModelResults` function can be used to read results from the `par`, `std`, 
`Gmacsall.std`, and `gmacs.rep1` files for multiple model runs:

```
fldrs_ = c(A=path_to_ModelResultsA,
           B=path_to_ModelResultsB);
Ms = wtsGMACS::readModelResults(fldrs_);
```

where `fldrs_` is a named character vector (the names will be used as model names in the 
results list) and the `path_`'s are paths to the model run folders of interest. `Ms` will be a 
`gmacs_reslst` object (a list with of class "gmacs_reslst") that encompasses results from 
all the models of interest.

Use `?wtsGMACS::readModelResults` for more information.