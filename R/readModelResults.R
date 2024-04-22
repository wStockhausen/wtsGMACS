#' 
#' @title Read gmacs results files from a (possibly several) results folder(s)
#' @description Function to read a set of gmacs model results from a (possibly several) model run folder(s).
#' @returns a `gmacs_reslst` object: a named list (see [@details])
#' @details A `gmacs_reslst` object is a list with elements
#' \itemize{
#' \item{dfrPars - `gmacs_par` dataframe object with parameter and convergence info from the par file(s), idenitified by `case`}
#' \item{dfrStds - `gmacs_std` dataframe object, with parameter and sd-variable estimates and uncertainty info from the std file(s), idenitified by `case`}
#' \item{dfrAllStds - `gmacs_allstd` dataframe object with parameter and sd-variable estimates and uncertainty info from the Gmacsall.std file(s), identified by case}
#' \item{repsLst - `gmacs_repslst` object, identified by `case`} 
#' }
#' If `fldrs` is a named vector, then the returned 
#' elements of repLsts are correspondingly named. If not, the folder names are used. For 
#' the returned dataframes, the `case` column contains the relevant name.
#' 
#' @export
#' 
readModelResults<-function(fldrs){
  message("Reading par files")
  parfns = file.path(fldrs,"gmacs.par");
  if (!is.null(names(fldrs))) names(parfns) = names(fldrs);
  dfrPars = readParFile(parfns);
  message("Reading std files")
  stdfns = file.path(fldrs,"gmacs.std");
  if (!is.null(names(fldrs))) names(stdfns) = names(fldrs); 
  dfrStds = readStdFile(stdfns);
  message("Reading Gmacsall.std files")
  stdfns = file.path(fldrs,"Gmacsall.std");
  if (!is.null(names(fldrs))) names(stdfns) = names(fldrs); 
  dfrAllStds = readGmacsAllStdFile(stdfns);
  
  repsLst = list();
  for (f in 1:length(fldrs)){
    fldr = fldrs[f];
    message("Reading gmacs.rep1 file from '",fldr,"'\n",sep="");
    iln<<-1;
    rep1 = readGmacsRep1(file.path(fldr,"gmacs.rep1"));
    if (is.null(names(fldr))||(names(fldr)=="")) {
      repsLst[[fldr]] = rep1;
    } else {
      repsLst[[names(fldr)]] = rep1;
    }
  }
  class(repsLst) = c("gmacs_repslst",class(repsLst));
  
  lst = list(dfrPars=dfrPars,
             dfrStds=dfrStds,
             repsLst=repsLst,
             dfrAllStds=dfrAllStds);
  class(lst) = c("gmacs_reslst",class(lst));
  return(lst);
}
# fldrs_ = c(A="testing/example_results",
#            B="testing/example_results");
# Ms = wtsGMACS::readModelResults(fldrs_);
# 
