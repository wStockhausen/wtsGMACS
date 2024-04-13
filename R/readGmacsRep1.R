#' 
#' @title Read a Gmacs gmacs.rep1 file
#' @description
#' Read a Gmacs .rep1 file and return a list of report objects.
#' @param fn - path to gmacs.rep1 file 
#' @param verbose - flag to print diagnostic info 
#' @return a named list of class `rep1` 
#' @details Must define `iln=1` in global environment before calling ths function. 
#' The returned object has class \code{rep1}` and inherits from \code{list}.
#' 
#' @examplesIf FALSE
#' # example code
#'   fn = "../../ModelRuns/TannerCrab24_01/gmacs.rep1";
#'   iln = 1; #--must set iln=1 to start!!
#'   rep = readGmacsRep1(fn,verbose=FALSE);
#' 
#' @import stringr
#' 
#' @export
#' 
readGmacsRep1<-function(fn,verbose=FALSE){
  lns = readLines(fn);#--read file
  nls = length(lns);
  
  out = list();
  ln = stringr::str_trim(lns[iln]);
  while (iln<nls){
    while((nchar(ln)==0)||stringr::str_starts(ln,"#")){
      advance();
      ln = stringr::str_trim(lns[iln]);
    }
    splt = stringr::str_trim(stringr::str_split_1(ln,":"));
    if (isKeyWord(splt[2])){
      objname = stringr::str_remove(splt[1],"--");
      advance();
      if (splt[2]=="list"){
        out[[objname]] = parseList(lns,verbose);
      } else if (splt[2]=="vector") {
        out[[objname]] = parseVector(lns,verbose);
      } else if (splt[2]=="matrix") {
        out[[objname]] = parseMatrix(lns,verbose);
      } else if (splt[2]=="dataframe") {
        out[[objname]] = parseDataframe(lns,verbose);
      }
    } else {
      out[[splt[1]]] = parseVal(splt[2]);
    }
    advance();
    if (verbose) cat("in parseAllout",iln," ");
    if (iln<nls) {
      ln = stringr::str_trim(lns[iln]);
      if (verbose) cat(ln,"\n")
    }
  }#--while (iln<nls)
  class(out) <- c("rep1",class(out));
  return(out);
}
