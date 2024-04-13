#' 
#' @title Create a list of rep1 objects
#' @description Function to create a list of rep1 objects.
#' @param pns - (possibly named) vector of paths to gmacs.rep1 files.
#' @param verbose - flag to print diagnostic info
#' @return named list of `rep1` objects.
#' 
#' @details If `pns` is a named vector, then the names are used as 
#' names in the return list. Otherwise, names for the returned list 
#' are derived from the parent folder of each gmacs.rep1 file.
#' 
#' @importFrom stringr str_remove
#' 
#' @export
#' 
getListOfRep1Objects<-function(pns,
                              verbose=FALSE){
  lst = list();
  cases = names(pns);
  pns = stringr::str_remove(pns,"gmacs.rep1");
  if (is.null(cases)) cases = basename(pns);
  for (i in 1:length(pns)){
    message(paste0("Attempting to read gmacs.rep1 in '",pns[i],"'."));
    cs = cases[i];
    pn = pns[i];
    fn = file.path(pn,"gmacs.rep1");
    if (!file.exists(fn)) {
      str = paste0("File '",fn,"' does not exist!")
      warning(str);
    } else {
      iln<<-1;
      lst[[cs]] = readGmacsRep1(fn,verbose);
    }
  }
  return(lst);
}
#lst = getListOfRep1Objects("testing/rep1file");
