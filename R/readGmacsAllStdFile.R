#'
#'@title Create a `gmacs_allstd` dataframe from GMACS Gmacsall.std files
#'
#'@description Function to create a `gmacs_allstd` dataframe from Gmacsall.std files.
#'
#'@param stdfn - path/filename of Gmacsall.std file (or list or vector of such)
#'@param verbose - flag to print diagnostics
#'
#'@returns a `gmacs_allstd` object (a [tibble] dataframe of class 'gmacs_allstd') corresponding to the 
#'std file(s)
#'
#'@details The returned tibble as columns
#'\itemize{ 
#'  \item{case - model case name (taken from the list or vector element name) or number. Value is 'gmacs' if only a single unnamed filename is given.}
#'  \item{index - index of the parameter value in the std file}
#'  \item{param - name of the parameter as given in the std file}
#'  \item{name - parsed parameter name, dropping any `[...]` index notation}
#'  \item{idx - the `...` within any `[...]` in the parameter name, or NA if none}
#'  \item{est - parameter estimate or convergence quantity value}
#'  \item{std - estimated (delta-method) standard error}
#'} 
#' 
#' @import dplyr
#' @import stringr
#' @importFrom tibble tibble 
#' 
#'@export
#' 
readGmacsAllStdFile<-function(stdfn=NULL,verbose=FALSE){
  lstAll = list();
  if (is.list(stdfn)||(length(stdfn)>1)){
    stdfn = as.list(stdfn);
    for (i in 1:length(stdfn)){
      dfr = readGmacsAllStdFile(stdfn[[i]]);
      if (!is.null(dfr)){
        if (!is.null(names(stdfn)[i])) {
          dfr$case = names(stdfn)[i];
        } else {
          dfr$case = paste("case",i);
        }
        lstAll[[i]] = dfr;
      }
      rm (dfr);
    }
    dfrAll = dplyr::bind_rows(lstAll);
    return(dfrAll);
  } else {
    if (!file.exists(stdfn)) {
      warning(paste0("Std file '",stdfn,"' does not exist. Returning NULL."));
      return(NULL);
    }
  
    case_ = names(stdfn);
    if (is.null(case_)) case_ = "gmacs";
    
  dfr = readr::read_delim(stdfn,delim=" ",skip=2,
                          col_names=c("index","par_no","name","est","std"),
                          show_col_types=verbose) |> 
          dplyr::mutate(case=case_,.before=1);
  class(dfr)<-c("gmacs_allstd",class(dfr));
    
    return(dfr)
  }
}
stdfns = c(A="testing/example_results/Gmacsall.std",
           B="testing/example_results/Gmacsall.std");
dfr = readGmacsAllStdFile(stdfns);
dfr = readGmacsAllStdFile(stdfns[["A"]]);
dfr = readGmacsAllStdFile(c(C=stdfns[["A"]]));

