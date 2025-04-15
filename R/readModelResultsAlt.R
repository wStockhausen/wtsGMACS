#' 
#' @title Read gmacs results files from a (possibly several) results folder(s)
#' @description Function to read a set of gmacs model results from a (possibly several) model run folder(s).
#' @param fldrs - a vector of folder names, each with the results from a single model run
#' @param verbose - flag (T/F) to print diagnostic info
#' @returns a `gmacs_reslstAlt` object: a named list (see [@details])
#' @details A `gmacs_reslstAlt` object is a named list of lists with elements
#' \itemize{
#' \item{case - name for model run}
#' \item{dfrPars - a `gmacs_par` dataframe object with parameter and convergence info from the par file(s), idenitified by `case`}
#' \item{dfrStds - a `gmacs_std` dataframe object, with parameter and sd-variable estimates and uncertainty info from the std file(s), idenitified by `case`}
#' \item{dfrAllStds - a `gmacs_allstd` dataframe object with parameter and sd-variable estimates and uncertainty info from the Gmacsall.std file(s), identified by case}
#' \item{rep - a gmacs_rep1 object (see [readGmacsRep1()])} 
#' \item{out - a gmacs_allout object (see [readGmacsAllout()])} 
#' }
#' If `fldrs` is a named vector, then the returned 
#' elements of the `gmacs_reslstAlt` object are correspondingly named. If not, the folder names are used. For 
#' the returned dataframes, the `case` column also contains the relevant name.
#' 
#' @export
#' 
readModelResultsAlt<-function(fldrs,verbose=FALSE){
  nf = length(fldrs);
  cases = unname(fldrs);
  if (!is.null(names(fldrs)) ) cases = names(fldrs);
  lstMRs = list();
  for (f in 1:nf){
    message("Reading results from '",fldrs[f],"'\n",sep="");
    ##--read gmacs.par file----
    message("Reading par file")
    parfn = file.path(fldrs[f],"gmacs.par");
    names(parfn) = cases[f];
    dfrPars = readParFile(parfn);
    
    ##--read gmacs.std file----
    message("Reading std file")
    stdfn = file.path(fldrs[f],"gmacs.std");
    names(stdfn) = cases[f]; 
    dfrStds = readStdFile(stdfn);
    
    ##--read Gmacsall.std file----
    message("Reading Gmacsall.std file")
    stdAllfn = file.path(fldrs[f],"Gmacsall.std");
    names(stdAllfn) = cases[f]; 
    dfrAllStds = readGmacsAllStdFile(stdAllfn);
  
    ##--read gmacs.rep1 files----
    message("Reading gmacs.rep1 file from '",fldrs[f],"'\n",sep="");
    iln<<-1;
    rep1 = readGmacsRep1(file.path(fldrs[f],"gmacs.rep1"));

    ##--read Gmacsall.out files----
    message("Reading Gmacsall.out file from '",fldrs[f],"'\n",sep="");
    iln<<-1;
    out1 = readGmacsAllout(file.path(fldrs[f],"Gmacsall.out"));
    
    lstMRs[[cases[f]]] = list(case=cases[f],
                              dfrPars=dfrPars,
                              dfrAllStds=dfrAllStds,
                              rep=rep1,
                              out=out1);
    rm(dfrPars,dfrAllStds,rep1,out1);
  }#--f loop
  class(lstMRs) = c("gmacs_reslstAlt",class(lstMRs));
  return(lstMRs);
}
# fldrs_ = c(A="testing/example_results",
#            B="testing/example_results");
# Ms = readModelResultsAlt(fldrs_);

