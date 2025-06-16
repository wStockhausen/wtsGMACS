# README


-   [wtsGMACS](#wtsgmacs)
    -   [Installation](#installation)
        -   [Dependencies](#dependencies)
    -   [How to use](#how-to-use)
    -   [NOAA Disclaimer](#noaa-disclaimer)

<!--DO NOT VIEW THIS FILE USING THE VISUAL EDITOR!! (seems to screw things up)-->
<!-- README.md is generated from README.qmd. Please edit README.qmd, then render README.md using `quarto render README.qmd` in a terminal window. -->
<!-- use `quarto render README.qmd` in the terminal window to build README.md prior to committing to keep README.md up-to-date-->
<!-- don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.-->

# wtsGMACS

`wtsGMACS` is an R package to deal with GMACS assessment data and
results.

## Installation

After installing the required packages (see below), this package can be
installed using:

    devtools::install_github("wStockhausen/wtsGMACS")

### Dependencies

This R package requires the following R packages be installed (links are
to repositories):

-   tcsamFunctions [https://github.com/wStockhausen/tcsamFunctions]
-   wtsPlots [https://github.com/wStockhausen/wtsPlots]
-   wtsUtilities [https://github.com/wStockhausen/wtsUtilities]

The packages above can be installed using

`devtools::install_github("wStockhausen/pkg_name")`

where `pkg_name` is the name of the package above. To avoid error
messages when installing the packages, install them in the reverse order
listed above.

Other required packages available on CRAN include:

-   dplyr
-   ggplot2
-   graphics
-   magrittr
-   RColorBrewer
-   readr
-   reshape2
-   stats
-   tibble
-   tidyr

## How to use

The `runGMACS` function can be used to run a GMACS model, as in:

    wtsGMACS::runGMACS(runpath=path_to_run_folder,
                       runCmds= list(path2exe=path_to_exe,
                                     path2dat=path_to_gmacs_dat_file);

In the above, `path_to_run_folder` is a path (absolute or relative to
the R working directory) to the folder in which you want the model to
run, `path_to_exe` is a path to the gmacs executable, and
`path_to_gmacs_dat_file` is a path to the `gmacs.dat` file describing
the model run,

Use `?wtsGMACS::runGMACS` for more information.

The `runJitter` function can be used to evaluate a series of model runs
under parameter jittering. Use `?wtsGMACS::runJitter` for more
information.

The `readModelResults` function can be used to read results from the
`par`, `std`, `Gmacsall.std`, and `gmacs.rep1` files for multiple model
runs:

    fldrs_ = c(A=path_to_ModelResultsA,
               B=path_to_ModelResultsB);
    Ms = wtsGMACS::readModelResults(fldrs_);

where `fldrs_` is a named character vector (the names will be used as
model names in the results list) and the `path_`’s are paths to the
model run folders of interest. `Ms` will be a `gmacs_reslst` object (a
list with of class “gmacs_reslst”) that encompasses results from all the
models of interest.

Use `?wtsGMACS::readModelResults` for more information.

------------------------------------------------------------------------

## NOAA Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)

<!-- badges: start -->
<!-- badges: end -->

<figure>
<a
href="https://github.com/wStockhausen/rtmbGMACS/actions/workflows/pages/pages-build-deployment"><img
src="https://github.com/wStockhausen/rtmbGMACS/actions/workflows/pages/pages-build-deployment/badge.svg?branch=gh-pages"
alt="pages-build-deployment" /></a>
<figcaption>pages-build-deployment</figcaption>
</figure>
